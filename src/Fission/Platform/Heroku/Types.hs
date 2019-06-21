module Fission.Platform.Heroku.Types
  ( ID (..)
  , Password (..)
  , Region (..)
  ) where

import RIO

import Database.Selda (SqlType)
import Data.Aeson
import Data.Swagger as Swagger

import qualified Fission.Internal.Schema as Schema

newtype ID = ID { getID :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString

newtype Password = Password { getPassword :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString

data Region
  = California
  | Dublin
  | Frankfurt
  | Oregon
  | Singapore
  | Sydney
  | Tokyo
  | Virginia
  deriving ( Show
           , Read
           , Eq
           , Enum
           , Generic
           , Bounded
           , SqlType
           )

instance ToJSON Region where
  toJSON = \case
    California -> String "amazon-web-services::us-west-1"
    Dublin     -> String "amazon-web-services::eu-west-1"
    Frankfurt  -> String "amazon-web-services::eu-central-1"
    Oregon     -> String "amazon-web-services::us-west-2"
    Singapore  -> String "amazon-web-services::ap-southeast-1"
    Sydney     -> String "amazon-web-services::ap-southeast-2"
    Tokyo      -> String "amazon-web-services::ap-northeast-1"
    Virginia   -> String "amazon-web-services::us-east-1"

instance FromJSON Region where
  parseJSON = withText "Region" $ \case
    "amazon-web-services::us-west-1"      -> return California
    "amazon-web-services::eu-west-1"      -> return Dublin
    "amazon-web-services::eu-central-1"   -> return Frankfurt
    "amazon-web-services::us-west-2"      -> return Oregon
    "amazon-web-services::ap-southeast-1" -> return Singapore
    "amazon-web-services::ap-southeast-2" -> return Sydney
    "amazon-web-services::ap-northeast-1" -> return Tokyo
    "amazon-web-services::us-east-1"      -> return Virginia
    bad -> fail $ "Invalid region: " <> show bad

instance ToSchema Region where
  declareNamedSchema = genericDeclareNamedSchema
    $ defaultSchemaOptions { Swagger.fieldLabelModifier = show . toJSON }
