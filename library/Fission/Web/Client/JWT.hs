module Fission.Web.Client.JWT
  ( mkAuthReq
  , getSigAuth
  , getRegisterAuth
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Servant.API hiding (addHeader)
import           Servant.Client.Core
 
import           Fission.Prelude
 
import qualified Fission.Internal.Orphanage.ClientM ()
 
import qualified Fission.Internal.Base64     as B64
import qualified Fission.Internal.Base64.URL as B64.URL

import qualified Fission.Key      as Key
import           Fission.User.DID as DID

import           Fission.Authorization as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types as Key

import           Fission.Web.Auth.Types                      as Auth
import           Fission.Web.Auth.Token.JWT                  as JWT
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types as JWT.Typ
import qualified Fission.Web.Auth.Token.JWT.Signature.Types  as JWT.Signature
import qualified Fission.Web.Auth.Token.Bearer.Types         as Bearer

getSigAuth ::
  ( MonadIO    m
  , MonadTime  m
  , MonadThrow m
  , ServerDID  m
  )
  => m (AuthenticatedRequest Auth.HigherOrder)
getSigAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest Nothing \_ -> authReq)

getRegisterAuth ::
  ( MonadIO    m
  , MonadTime  m
  , MonadThrow m
  , ServerDID  m
  )
  => m (AuthenticatedRequest Auth.RegisterDID)
getRegisterAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest () \_ -> authReq)

mkAuthReq ::
  ( MonadIO   m
  , MonadTime m
  , ServerDID m
  )
  => m (Either Key.Error (Request -> Request))
mkAuthReq = do
  time       <- currentTime
  fissionDID <- getServerDID

  Key.readEd <&> \case
    Left err -> Left err
    Right sk -> Right \req -> addHeader "Authorization" encoded req
      where
        encoded = toUrlPiece $ Bearer.Token JWT {..} Nothing
        sig     = JWT.Signature.Ed25519 . Key.signWith sk $ B64.URL.encodeJWT header claims
        rawPK   = Ed25519.toPublic sk
 
        senderDID = DID
          { publicKey = Key.Public $ B64.toB64ByteString rawPK
          , algorithm = Key.Ed25519
          , method    = DID.Key
          }

        claims = JWT.Claims
          { sender   = senderDID
          , receiver = fissionDID
          , potency  = AppendOnly
          , scope    = "/"
          , proof    = RootCredential
          , nbf      = Nothing
          , exp      = addUTCTime (secondsToNominalDiffTime 30) time
          }

        header = JWT.Header
          { typ = JWT.Typ.JWT
          , alg = Key.Ed25519
          , cty = Nothing
          , uav = Authorization.latestVersion
          }
