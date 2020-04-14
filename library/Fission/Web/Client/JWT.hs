module Fission.Web.Client.JWT
  ( mkAuthReq
  , getSigAuth
  , getRegisterAuth
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified RIO.ByteString.Lazy   as Lazy

import           Servant.API hiding (addHeader)
import           Servant.Client.Core
 
import           Fission.Prelude

import qualified Fission.Key      as Key
import           Fission.User.DID as DID
 
import           Fission.Key.Asymmetric.Algorithm.Types as Key

import           Fission.Web.Auth.Types                as Auth
import           Fission.Web.Auth.JWT.Types            as JWT
import qualified Fission.Web.Auth.JWT.Header.Typ.Types as JWT.Typ
import qualified Fission.Web.Auth.JWT.Signature.Types  as JWT.Signature
import qualified Fission.Web.Auth.Token.Bearer.Types   as Bearer

import qualified Fission.Internal.Orphanage.ClientM ()
 
import qualified Fission.Internal.Base64 as B64
import qualified Fission.Internal.UTF8   as UTF8

getSigAuth ::
  ( MonadIO    m
  , MonadTime  m
  , MonadThrow m
  )
  => m (AuthenticatedRequest Auth.HigherOrder)
getSigAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest Nothing \_ -> authReq)

getRegisterAuth ::
  ( MonadIO    m
  , MonadTime  m
  , MonadThrow m
  )
  => m (AuthenticatedRequest Auth.RegisterDID)
getRegisterAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest () \_ -> authReq)

mkAuthReq :: (MonadIO m, MonadTime m) => m (Either Key.Error (Request -> Request))
mkAuthReq = do
  time <- currentTime
 
  Key.readEd <&> \case
    Left err -> Left err
    Right sk -> Right \req -> addHeader "Authorization" encoded req
      where
        encoded = toUrlPiece $ Bearer.Token JWT {..}
        sig     = JWT.Signature.Ed25519 $ Key.signWith sk toSign
        toSign  = Lazy.toStrict $ encode header <> "." <> encode claims
        rawPK   = Ed25519.toPublic sk
 
        did = DID
          { publicKey = Key.Public $ B64.toB64ByteString rawPK
          , algorithm = Key.Ed25519
          , method    = DID.Key
          }

        claims = JWT.Claims
          { iss = did
          , nbf = Nothing
          , exp = addUTCTime (secondsToNominalDiffTime 300) time
          }

        header = JWT.Header
          { typ = JWT.Typ.JWT
          , alg = Key.Ed25519
          , cty = Nothing
          }

