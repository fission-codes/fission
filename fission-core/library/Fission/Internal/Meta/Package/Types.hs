module Fission.Internal.Meta.Package.Types (Package (..)) where

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
