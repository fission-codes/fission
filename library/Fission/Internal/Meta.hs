module Fission.Internal.Meta
  ( Package (..)
  , package
  ) where

import Data.FileEmbed
import Data.Yaml as Yaml

import Fission.Prelude

data Package = Package
  { name    :: !(Maybe Text)
  , version :: !(Maybe Text)
  } deriving (Show, Eq)

instance FromJSON Package where
  parseJSON = withObject "Package" \obj -> do
    name    <- obj .:? "name"
    version <- obj .:? "version"
    return Package {..}

package :: Maybe Package
package =
  case Yaml.decodeEither' $(embedFile "./package.yaml") of
    Left  _err -> Nothing
    Right val  -> Just val
