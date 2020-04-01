module Fission.Web.Auth.JWT
  ( handler
  , validateJWT
  , module Fission.Web.Auth.JWT.Types
  , module Fission.Web.Auth.JWT.Error
  ) where

import           Database.Esqueleto (Entity (..))

import Control.Monad.Trans.Except

-- import qualified Data.ByteString.Char8  as Ch
-- import qualified Data.ByteString.Lazy   as BS.Lazy
-- import qualified Data.ByteString.Base64 as Base64

import qualified Crypto.PubKey.Ed25519 as Ed
import           Crypto.Error

import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import           Fission.Models
import           Fission.Web.Error.Class

import qualified Fission.Internal.Crypto as Crypto

import           Fission.Web.Auth.JWT.Types as JWT
import           Fission.Web.Auth.JWT.Error as JWT

import qualified Fission.User     as User
import           Fission.User.DID as DID

import           Fission.PublicKey.Types

import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

-- Reexport

import           Fission.Web.Auth.JWT.Types
import           Fission.Web.Auth.JWT.Error

handler ::
  ( MonadTime        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m (Entity User)
handler token@(Auth.Bearer.Token rawToken) =
  validateJWT token >>= \case
    Left err -> do
      logWarn <| "Failed login with token " <> rawToken
      throwM err

    Right pk -> do
      runDB <| User.getByPublicKey pk >>= \case
        Nothing -> throwM <| toServerError JWT.NoUser
        Just usr -> return usr

validateJWT :: MonadTime m => Auth.Bearer.Token -> m (Either JWT.Error PublicKey)
validateJWT rawToken = runExceptT do
  Token {..} <- except $ parseJWT rawToken
  ExceptT $ validateTime claims <&> \case
    Left err -> Left err
    Right _  -> Right $ iss claims

-- FIXME rename to vakluidate
parseJWT :: Auth.Bearer.Token -> Either JWT.Error JWT.Token
parseJWT (Auth.Bearer.Token rawToken) = do
  token@Token {..} <- either (\_ -> Left ParseError) Right $
    eitherDecode $ Lazy.fromStrict rawToken
   
  -- validateHeader header

  let
    Claims {..} = claims
    content     = Lazy.toStrict $ encode header <> "." <> encode claims
 
  pubkey64 <- DID.toPubkey iss -- FIXME right, we do want the `did:key` here
  void $ validateSig content pubkey64 (encodeUtf8 $ textDisplay $ displayShow sig) -- FIXME

  Right token

-- validateHeader :: Header -> Either JWT.Error ()
-- validateHeader Header {..} =
--   case (typ, alg) of
--     (JWT, JWT.Ed25519) -> Right ()
--     -- (JWT, JWT.RSA2048) -> Right ()
--     -- (JWT, _)           -> Left UnsupportedAlg
--     _                  -> Left BadHeader

-- TODO Probbaly also should be called in the FromJSON

validateTime :: MonadTime m => JWT.Claims -> m (Either JWT.Error ())
validateTime JWT.Claims { exp, nbf } = do
  time <- currentTime
  return case (time > exp, nbf) of
    (True, _        ) -> Left JWT.Expired
    (_,    Just nbf') -> if time < nbf' then Left JWT.TooEarly else ok
    _                 -> ok

validateSig :: ByteString -> ByteString -> ByteString -> Either JWT.Error ()
validateSig content pubkey64 sig64 =
  case (Crypto.base64ToEdPubKey pubkey64, Crypto.base64ToSignature sig64) of
    (CryptoPassed pk, CryptoPassed sig) ->
      case Ed.verify pk (Crypto.pack content) sig of
        False -> Left IncorrectSignature
        True -> Right ()
 
    _ ->
      Left BadSignature
