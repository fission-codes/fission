module Fission.Web.API.App.Index.Payload.Types (Payload(..)) where

import           Data.Swagger    hiding (URL, url)

import           Fission.Prelude

import           Fission.URL

data Payload = Payload
  { urls       :: [URL]
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
  }
  deriving (Eq, Show)

instance FromJSON Payload where
  parseJSON = withObject "Payload" \obj -> do
    urls       <- obj .: "urls"
    insertedAt <- obj .: "insertedAt"
    modifiedAt <- obj .: "modifiedAt"

    return Payload {..}

instance ToJSON Payload where
  toJSON Payload {..} = object
    [ "urls"       .= urls
    , "insertedAt" .= insertedAt
    , "modifiedAt" .= modifiedAt
    ]

instance ToSchema Payload where
  declareNamedSchema _ = do
    urls'       <- declareSchemaRef $ Proxy @[URL]
    insertedAt' <- declareSchemaRef $ Proxy @UTCTime
    modifiedAt' <- declareSchemaRef $ Proxy @UTCTime

    mempty
      |> type_      ?~ SwaggerObject
      |> properties .~
           [ ("urls", urls')
           , ("insertedAt", insertedAt')
           , ("modifiedAt", modifiedAt')
           ]
      |> required .~ ["username", "email"]
      |> description ?~ "Properties for a registered application"
      |> example ?~ toJSON Payload
        { urls       = [url]
        , insertedAt = fromSeconds 1620600000
        , modifiedAt = fromSeconds 1620610000
        }
      |> NamedSchema (Just "App Index Payload")
      |> pure

    where
      url = URL
        { domainName = DomainName "fission.codes"
        , subdomain  = Just $ Subdomain "my-cool-subdomain"
        }
