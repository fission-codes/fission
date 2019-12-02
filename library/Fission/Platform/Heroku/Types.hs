module Fission.Platform.Heroku.Types
  ( ID (..)
  , Password (..)
  , Region (..)
  ) where

import Data.Swagger as Swagger
import RIO.Partial (read)

-- Fission

import           Fission.Prelude
import qualified Fission.Storage.Persist as Persist


-- | Heroku add-on ID (from @addon-manifest.json@)
newtype ID = ID { getID :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString

-- | Heroku add-on password (from @addon-manifest.json@)
newtype Password = Password { getPassword :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString

-- | Location of Heroku server requesting keys
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
           )

instance ToJSON Region where
  toJSON = String . \case
    California -> "amazon-web-services::us-west-1"
    Dublin     -> "amazon-web-services::eu-west-1"
    Frankfurt  -> "amazon-web-services::eu-central-1"
    Oregon     -> "amazon-web-services::us-west-2"
    Singapore  -> "amazon-web-services::ap-southeast-1"
    Sydney     -> "amazon-web-services::ap-southeast-2"
    Tokyo      -> "amazon-web-services::ap-northeast-1"
    Virginia   -> "amazon-web-services::us-east-1"

instance FromJSON Region where
  parseJSON = withText "Region" <| \case
    "amazon-web-services::us-west-1"      -> return California
    "amazon-web-services::eu-west-1"      -> return Dublin
    "amazon-web-services::eu-central-1"   -> return Frankfurt
    "amazon-web-services::us-west-2"      -> return Oregon
    "amazon-web-services::ap-southeast-1" -> return Singapore
    "amazon-web-services::ap-southeast-2" -> return Sydney
    "amazon-web-services::ap-northeast-1" -> return Tokyo
    "amazon-web-services::us-east-1"      -> return Virginia

    bad -> fail ("Invalid region: " <> show bad)

instance ToSchema Region where
  declareNamedSchema = genericDeclareNamedSchema <| defaultSchemaOptions
    { Swagger.constructorTagModifier = (\str -> take (length str - 1) str)
                                     . drop 8
                                     . show
                                     . toJSON
                                     . (read :: String -> Region)
    }

Persist.generateInstances "Region"
