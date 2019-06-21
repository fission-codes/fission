module Fission.Web.Ping.Types (Pong (..)) where

import RIO
import Servant

import Data.Aeson
import Data.Swagger hiding (name)
import Control.Lens

import qualified Fission.Internal.UTF8 as UTF8

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
     return $ NamedSchema (Just "Pong") $ mempty
            & type_ .~ SwaggerString
            & description ?~ "A simple response"

instance MimeRender PlainText Pong where
  mimeRender _proxy = UTF8.textToLazyBS . unPong
