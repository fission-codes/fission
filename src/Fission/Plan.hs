module Fission.Plan (Tier (..)) where

import RIO
import RIO.Text (toLower)

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Swagger as Swagger

-- import Fission.Internal.Schema as Schema

data Tier
  = Test
  | Free
  --  | Paid
  deriving ( Eq
           , Show
           , Generic
           )

makeLenses ''Tier

instance ToJSON Tier where
  toJSON = String . toLower . textDisplay . displayShow

instance FromJSON Tier where
  parseJSON (String str) =
    case str of
      "Test" -> pure Test
      "test" -> pure Test

      "Free" -> pure Free
      "free" -> pure Free

      other  -> cantParse other

  parseJSON other = cantParse other

instance ToSchema Tier where
  declareNamedSchema = genericDeclareNamedSchema
    $ defaultSchemaOptions { Swagger.fieldLabelModifier = show . toJSON }

cantParse :: (Monad m, Show a) => a -> m b
cantParse other = fail $ "Unable to parse " <> show other
