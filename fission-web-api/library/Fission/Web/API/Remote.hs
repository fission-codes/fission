module Fission.Web.API.Remote
  ( Remote (..)
  , fromText
  , toBaseUrl
  -- * Known environments
  , production
  , staging
  , development
  )  where

import qualified RIO.Text            as Text
import           Servant.Client.Core

import           Fission.Prelude

-- | Remote environment
data Remote
  = Production
  | Staging
  | LocalDev
  | FullMock
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

    "mock"        -> pure FullMock

    custom        -> Custom <$> parseBaseUrl (Text.unpack custom)

toBaseUrl :: Remote -> Maybe BaseUrl
toBaseUrl = \case
  Production -> Just production
  Staging    -> Just staging
  LocalDev   -> Just development
  FullMock   -> Nothing
  Custom url -> Just url

production :: BaseUrl
production = BaseUrl Https "runfission.com" 443  ""

staging :: BaseUrl
staging = BaseUrl Https "runfission.net" 443 ""

development :: BaseUrl
development = BaseUrl Http "localhost" 1337 ""
