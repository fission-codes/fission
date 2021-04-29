module Fission.Web.API.Remote
  ( Remote (..)
  , fromText
  , toBaseUrl
  , toURL
  , toNameService
  -- * Known environments
  , production
  , staging
  , development
  )  where

import qualified RIO.Text            as Text
import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.URL         as URL
import           Fission.URL.Types

-- | Remote environment
data Remote
  = Production
  | Staging
  | LocalDev
  | Custom BaseUrl
  deriving (Show, Eq)


instance Display Remote where
  display = displayShow

instance ToJSON Remote where
  toJSON = String . textDisplay

instance FromJSON Remote where
  parseJSON = withText "Remote" \txt ->
    case fromText txt of
      Nothing     -> fail "Not a valid remote"
      Just remote -> pure remote

fromText :: Text -> Maybe Remote
fromText txt =
  case Text.strip $ Text.toLower txt of
    "production"  -> pure Production
    "prod"        -> pure Production

    "staging"     -> pure Staging

    "development" -> pure LocalDev
    "dev"         -> pure LocalDev

    custom        -> Custom <$> parseBaseUrl (Text.unpack custom)

toBaseUrl :: Remote -> BaseUrl
toBaseUrl = \case
  Production -> production
  Staging    -> staging
  LocalDev   -> development
  Custom url -> url

toURL :: Remote -> URL
toURL remote = URL.fromBaseUrl $ toBaseUrl remote

production :: BaseUrl
production = BaseUrl Https "runfission.com" 443  ""

staging :: BaseUrl
staging = BaseUrl Https "runfission.net" 443 ""

development :: BaseUrl
development = BaseUrl Http "localhost" 1337 ""

toNameService :: Remote -> URL
toNameService = \case
  Production -> URL "fission.name"    Nothing
  Staging    -> URL "fissionuser.net" Nothing
  LocalDev   -> URL "localhost"       Nothing
  Custom url -> URL.fromBaseUrl url
