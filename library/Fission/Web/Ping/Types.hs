module Fission.Web.Ping.Types (Pong (..)) where

import           Data.Swagger hiding (name)
import           Servant

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

-- | A dead-simple text wrapper.
--   Primarily exists for customized instances.
newtype Pong = Pong { unPong :: Text }
  deriving         ( Eq
                   , Show
                   )
  deriving newtype ( IsString
                   , FromJSON
                   , ToJSON
                   )

instance ToSchema Pong where
  declareNamedSchema _ =
    mempty
      |> type_       ?~ SwaggerString
      |> description ?~ "A simple response"
      |> example     ?~ toJSON (Pong "pong")
      |> NamedSchema (Just "Pong")
      |> pure

instance MimeRender PlainText Pong where
  mimeRender _proxy = UTF8.textToLazyBS . unPong
