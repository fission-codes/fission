module Fission.Web.Client.JWT
  ( mkAuthReq
  , ucan
  ) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Servant.API hiding (addHeader)
import           Servant.Client.Core

import           Fission.Prelude
 
import qualified Fission.Internal.Orphanage.ClientM ()
 
import qualified Fission.Internal.Base64.URL as B64.URL

import qualified Fission.Key      as Key
import           Fission.User.DID as DID

import           Fission.Authorization as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types as Key

import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Web.Auth.Token.JWT                  as JWT
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types as JWT.Typ
import qualified Fission.Web.Auth.Token.JWT.Signature.Types  as JWT.Signature
import qualified Fission.Web.Auth.Token.Bearer.Types         as Bearer
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.Web.Client.Auth

-- NOTE Probably can be changed to `hoistClientMonad` at call site
mkAuthReq ::
  ( MonadIO      m
  , MonadTime    m
  , ServerDID    m
  , MonadWebAuth m Ed25519.SecretKey
  )
  => m (Request -> Request)
mkAuthReq = do
  now        <- currentTime
  fissionDID <- getServerDID
  sk         <- getAuth

  let
    jwt =
      ucan now fissionDID sk RootCredential

    raw =
      jwt
        |> encode
        |> Lazy.toStrict
        |> decodeUtf8Lenient
        |> Text.dropPrefix "\""
        |> Text.dropSuffix "\""
 
    encoded =
      toUrlPiece $ Bearer.Token jwt (JWT.contentOf raw)
 
  return \req -> addHeader "Authorization" encoded req

ucan :: UTCTime -> DID -> Ed25519.SecretKey -> Proof -> JWT
ucan now fissionDID sk proof = JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

    senderDID = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic sk
      , method    = DID.Key
      }

    claims = JWT.Claims
      { sender   = senderDID
      , receiver = fissionDID

      , potency  = AppendOnly
      , resource = Subset (FissionFileSystem "/")
      , proof    = proof
     
      -- Accounting for clock drift
      , nbf      = addUTCTime (secondsToNominalDiffTime (-30)) now
      , exp      = addUTCTime (secondsToNominalDiffTime   30)  now
      }

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }
