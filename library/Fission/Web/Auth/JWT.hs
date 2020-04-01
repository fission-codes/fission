module Fission.Web.Auth.JWT
  ( handler
  , validateJWT
  , module Fission.Web.Auth.JWT.Types
  , module Fission.Web.Auth.JWT.Error
  ) where

import           Control.Monad.Trans.Except

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
      logWarn $ "Failed login with token " <> rawToken
      throwM err

    Right DID {..} -> do
      runDB $ User.getByPublicKey publicKey >>= \case
        Nothing  -> throwM $ toServerError JWT.NoUser
        Just usr -> return usr

validateJWT :: MonadTime m => Auth.Bearer.Token -> m (Either JWT.Error DID)
validateJWT rawToken = runExceptT do
  Token {claims} <- except $ parseJWT rawToken
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
    Claims {iss = DID {publicKey}} = claims
    content     = Lazy.toStrict $ encode header <> "." <> encode claims
 
  -- pubkey64 <- publicKey iss -- FIXME right, we do want the `did:key` here
  void $ validateSig content publicKey (encodeUtf8 $ textDisplay $ displayShow sig) -- FIXME

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

validateSig :: ByteString -> PublicKey -> ByteString -> Either JWT.Error ()
validateSig content (PublicKey pk64) sig64 =
  case (cryptoPK, cryptoSig) of
    (CryptoPassed pk, CryptoPassed sig) ->
      if Ed.verify pk (Crypto.pack content) sig
        then ok
        else Left IncorrectSignature

    _ ->
      Left BadSignature
  where
    cryptoPK  = Crypto.base64ToEdPubKey $ encodeUtf8 pk64
    cryptoSig = Crypto.base64ToSignature sig64
