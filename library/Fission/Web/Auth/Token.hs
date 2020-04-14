module Fission.Web.Auth.Token
  ( get
  , module Fission.Web.Auth.Token.Types
  ) where

import           Network.Wai
import           Servant.API

import           Fission.Prelude
import           Fission.Web.Auth.Token.Types

get :: Request -> Maybe Token
get req = do
  auth <- case lookup "Authorization" headers of
    Just token -> Just token
    Nothing    -> lookup "authorization" headers

  case parseUrlPiece $ decodeUtf8Lenient auth of
    Right token -> Just token
    Left  _     -> Nothing

  where
    headers = requestHeaders req
