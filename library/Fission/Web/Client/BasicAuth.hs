module Fission.Web.Client.BasicAuth (getBasicAuth) where

import Fission.Prelude

import Servant hiding (addHeader)
import Servant.Client.Core

import qualified Data.ByteString.Base64 as Base64
import           Fission.Web.Auth.Types as Auth

getBasicAuth :: BasicAuthData -> AuthenticatedRequest (Auth.HigherOrder)
getBasicAuth auth = mkAuthenticatedRequest (Just auth) addBasicAuth

addBasicAuth :: Maybe BasicAuthData -> Request -> Request
addBasicAuth Nothing     req = req
addBasicAuth (Just auth) req = addHeader "Authorization" token req
  where
    token = decodeUtf8Lenient <| encodeBasicAuth auth

encodeBasicAuth :: BasicAuthData -> ByteString
encodeBasicAuth auth = "Basic " <> Base64.encode (basicAuthUsername auth <> ":" <> basicAuthPassword auth)
