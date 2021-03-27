module Fission.Web.Client.JWT (mkAuthReq) where

-- import qualified RIO.ByteString.Lazy                 as Lazy
-- import qualified RIO.Text                            as Text

import qualified Crypto.PubKey.Ed25519               as Ed25519

import           Servant.API                         hiding (addHeader)
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization               as Authorization

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT          as JWT

import           Fission.Web.Client.Auth

import qualified Fission.Internal.Base64.URL         as Base64.URL

-- NOTE Probably can be changed to `hoistClientMonad` at call site
mkAuthReq ::
  ( MonadTime m
  , ServerDID m
  , MonadWebAuth m Ed25519.SecretKey
  )
  => JWT.Proof
  -> m (Request -> Request)
mkAuthReq proof = do
  now        <- currentTime
  fissionDID <- getServerDID
  sk         <- getAuth

  let
    jwt@JWT{header, claims} = JWT.simpleWNFS now fissionDID sk [] proof
    raw                     = RawContent $ Base64.URL.encodeJWT header claims
    encoded                 = toUrlPiece $ Bearer.Token jwt raw

  return \req -> addHeader "Authorization" encoded req
