module Fission.Web.Client.JWT (mkAuthReq) where

import qualified RIO.ByteString.Lazy                 as Lazy
import qualified RIO.Text                            as Text

import qualified Crypto.PubKey.Ed25519               as Ed25519

import           Servant.API                         hiding (addHeader)
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization               as Authorization

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT          as JWT

import           Fission.Web.Client.Auth

-- NOTE Probably can be changed to `hoistClientMonad` at call site
mkAuthReq ::
  ( MonadTime m
  , ServerDID m
  , MonadWebAuth m Ed25519.SecretKey
  )
  => m (Request -> Request)
mkAuthReq = do
  now        <- currentTime
  fissionDID <- getServerDID
  sk         <- getAuth

  let
    jwt =
      JWT.simpleWNFS now fissionDID sk [] RootCredential

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
