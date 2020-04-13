module Fission.Web.Auth.Token
  ( get
  , module Fission.Web.Auth.Token.Types
  ) where

import qualified RIO.ByteString      as BS
import qualified RIO.ByteString.Lazy as Lazy
import           Network.Wai

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Auth.Token.Types
import qualified Fission.Web.Auth.Token.Basic.Types as Basic

get :: Request -> Maybe Token
get req = do
  let headers = requestHeaders req
 
  rawToken <- case lookup "Authorization" headers of
    Just token -> token
    Nothing    -> lookup "authorization" headers

  case BS.stripPrefix "Basic " rawToken of
    Just basic' ->
      Just . Basic $ Basic.Token basic'

    Nothing -> do
      let normalizedJSON = "\"" <> UTF8.stripQuotesBS rawToken <> "\""
      Bearer <$> decode' (Lazy.fromStrict normalizedJSON)
