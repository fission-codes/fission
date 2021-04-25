module Fission.App.Types
  ( module Fission.App.Name
  , App(..)
  ) where

import Data.Swagger hiding (URL, url)

import Fission.Prelude

import Fission.App.Name
import Fission.URL


data App = App
  { urls :: [URL]
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
  }
  -- What about deriving Json of normal data?
  -- deriving (Eq, Generic, Show, ToJSON)
  deriving (Eq, Show)



instance FromJSON App where
  parseJSON = withObject "App" \obj -> do
    urls        <- obj .: "urls"
    insertedAt <- obj .: "insertedAt"
    modifiedAt <- obj .: "modifiedAt"

    return App {..}

instance ToJSON App where
  toJSON App {..} = object
    [ "urls"        .= urls
    , "insertedAt" .= insertedAt
    , "modifiedAt" .= modifiedAt
    ]

instance ToSchema App where
  declareNamedSchema _ = do
    urls'       <- declareSchemaRef (Proxy :: Proxy [URL])
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
      |> example ?~ toJSON App
        { urls       = [ URL
                            (DomainName "fission.codes")
                            (Just $ Subdomain "my-cool-subdomain")
                        ]
        , insertedAt = fromSeconds 0
        , modifiedAt = fromSeconds 20
        }
      |> NamedSchema (Just "App")
      |> pure
