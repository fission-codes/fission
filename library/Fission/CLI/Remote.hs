module Fission.CLI.Remote
  ( fromText
  , toBaseUrl
  -- * Known environments
  , production
  , staging
  , development
  -- * Reexports
  , module Fission.CLI.Remote.Types
  )  where

import qualified RIO.Text                 as Text
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.CLI.Remote.Types

fromText :: Text -> Maybe Remote
fromText txt =
  case Text.strip $ Text.toLower txt of
    "production"  -> pure Production
    "prod"        -> pure Production

    "staging"     -> pure Staging

    "development" -> pure Development
    "dev"         -> pure Development

    "mock"        -> pure FullMock

    custom        -> Custom <$> parseBaseUrl (Text.unpack custom)

toBaseUrl :: Remote -> Maybe BaseUrl
toBaseUrl = \case
  Production  -> Just production
  Staging     -> Just staging
  Development -> Just development
  FullMock    -> Nothing
  Custom url  -> Just url

production :: BaseUrl
production = BaseUrl Https "runfission.com" 443  ""

staging :: BaseUrl
staging = BaseUrl Https "runfission.net" 443 ""

development :: BaseUrl
development = BaseUrl Http "localhost" 1337 ""
