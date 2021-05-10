module Fission.App.Types
  ( module Fission.App.Name
  , Payload(..)
  ) where

import           Data.Swagger     hiding (URL, url)

import           Fission.Prelude

import           Fission.App.Name
import           Fission.URL


data Payload = Payload
  { urls       :: [URL]
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
  }
  -- What about deriving Json of normal data?
  -- deriving (Eq, Generic, Show, ToJSON)
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
        { urls       = [ URL
                            (DomainName "fission.codes")
                            (Just $ Subdomain "my-cool-subdomain")
                        ]
        , insertedAt = fromSeconds 0
        , modifiedAt = fromSeconds 20
        }
      |> NamedSchema (Just "Payload")
      |> pure
