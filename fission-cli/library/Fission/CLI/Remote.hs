module Fission.CLI.Remote
  ( getRemoteURL
  , getRemoteBaseUrl
  --
  , fromText
  , toBaseUrl
  -- * Known environments
  , production
  , staging
  , development
  -- * Reexports
  , module Fission.CLI.Remote.Class
  , module Fission.CLI.Remote.Types
  )  where

import qualified RIO.Text                 as Text
import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.URL              as URL
import           Fission.URL.Types

import           Fission.CLI.Remote.Class
import           Fission.CLI.Remote.Types

getRemoteBaseUrl :: MonadRemote m => m BaseUrl
getRemoteBaseUrl = toBaseUrl <$> getRemote

getRemoteURL :: MonadRemote m => m URL
getRemoteURL = toURL <$> getRemote

--

fromText :: Text -> Maybe Remote
fromText txt =
  case Text.strip $ Text.toLower txt of
    "production"  -> pure Production
    "prod"        -> pure Production

    "staging"     -> pure Staging

    "development" -> pure Development
    "dev"         -> pure Development

    custom        -> Custom <$> parseBaseUrl (Text.unpack custom)

toBaseUrl :: Remote -> BaseUrl
toBaseUrl = \case
  Production  -> production
  Staging     -> staging
  Development -> development
  Custom url  -> url

toURL :: Remote -> URL
toURL = URL.fromBaseUrl . toBaseUrl

--

production :: BaseUrl
production = BaseUrl Https "runfission.com" 443  ""

staging :: BaseUrl
staging = BaseUrl Https "runfission.net" 443 ""

development :: BaseUrl
development = BaseUrl Http "localhost" 1337 ""
