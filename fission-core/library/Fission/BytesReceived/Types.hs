module Fission.BytesReceived.Types (BytesReceived (..)) where

import           Data.Swagger    hiding (URL, url)

import           Fission.Prelude

newtype BytesReceived = BytesReceived { byteCount :: Natural }
  deriving (Show, Eq)

instance ToJSON BytesReceived where
  toJSON BytesReceived {..} = object [ "byteCount" .= byteCount ]

instance FromJSON BytesReceived where
  parseJSON =
    withObject "BytesReceived" \obj -> do
      byteCount <- obj .: "byteCount"
      return BytesReceived {..}

instance ToSchema BytesReceived where
  declareNamedSchema _ = do
    bytesSchema <- declareSchemaRef $ Proxy @Natural

    mempty
      |> type_       ?~ SwaggerObject
      |> properties  .~ [("byteCount", bytesSchema)]
      |> required    .~ ["byteCount"]
      |> description ?~ "Properties for a registered application"
      |> example     ?~ toJSON (BytesReceived 42)
      |> NamedSchema (Just "BytesReceived")
      |> pure
