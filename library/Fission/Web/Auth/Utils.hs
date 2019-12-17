module Fission.Web.Auth.Utils (getToken) where

import           Fission.Prelude

import qualified RIO.ByteString as BS
import           Network.Wai
import           Fission.Web.Auth.Types as Auth

getToken :: Request -> Auth.Token
getToken req = 
  case lookup "Authorization" (requestHeaders req) of
    Nothing -> Auth.None
    Just token -> case BS.stripPrefix "Basic " token of 
      Just basic' -> Auth.Basic <| Auth.BasicToken basic'
      Nothing -> case BS.stripPrefix "Bearer " token of
        Just bearer -> Auth.Bearer <| Auth.BearerToken bearer
        Nothing -> Auth.None
