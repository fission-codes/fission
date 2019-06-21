module Fission.Internal.Schema (fromJSON) where

import RIO

import Data.Aeson   (ToJSON (..))
import Data.Swagger
-- import Data.Swagger.Declare

-- fromJSON :: (Generic a, ToJSON a) => Proxy a -> Declare (Definitions Schema) NamedSchema
fromJSON = genericDeclareNamedSchema (defaultSchemaOptions { fieldLabelModifier = show . toJSON })
