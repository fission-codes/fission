module Fission.Web.API.App.Update.Streaming.Types where

import           Servant.API
import           Servant.Types.SourceT

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.URL.Types

import           Fission.Prelude

import qualified Fission.Web.API.Auth.Types as Auth
import           Fission.Web.API.Prelude





import           Data.Swagger               hiding (URL, url)
import           Servant.Types.SourceT      as S
import           Streamly.Prelude

type StreamingUpdate
  = Summary "" -- FIXME
  :> Description "" -- FIXME
  --
  :> Capture    "App URL"   URL
  :> Capture    "New CID"   IPFS.CID
  --
  :> Auth.HigherOrder
  :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO BytesReceived)


newtype BytesReceived = BytesReceived { byteCount :: Natural } -- FIXME is it bytes?
  deriving (Show, Eq)

instance ToJSON BytesReceived where
  toJSON BytesReceived {..} = object [ "bytes" .= byteCount ]

-- ToSchema Fission.Web.API.App.Update.Streaming.Types.BytesReceived

instance ToSchema BytesReceived where -- VERY MUCH FIXME
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
      |> example ?~ toJSON (BytesReceived 42)
      |> NamedSchema (Just "App Index Payload")
      |> pure
