module Fission.Web.Client.JWT (mkAuthReq) where

-- import qualified RIO.ByteString.Lazy                 as Lazy
-- import qualified RIO.Text                            as Text

import qualified Crypto.PubKey.Ed25519               as Ed25519

import           Servant.API                         hiding (addHeader)
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization               as Authorization

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.Ucan         as Ucan
import qualified Fission.Web.Auth.Token.Ucan.Types   as Fission

import           Fission.Web.Client.Auth

import qualified Web.Ucan.Internal.Base64.URL        as Base64.URL
import           Web.Ucan.Types

-- NOTE Probably can be changed to `hoistClientMonad` at call site
mkAuthReq ::
  ( MonadTime m
  , ServerDID m
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Fission.Proof
  -> m (Request -> Request)
mkAuthReq proof = do
  now        <- currentTime
  fissionDID <- getServerDID
  sk         <- getAuth

  let
    jwt@Ucan{header, claims} = Ucan.simpleWNFS now fissionDID sk [] proof
    raw                     = RawContent $ Base64.URL.encodeJWT header claims
    encoded                 = toUrlPiece $ Bearer.Token jwt raw

  return \req -> addHeader "Authorization" encoded req
