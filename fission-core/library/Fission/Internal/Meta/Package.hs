module Fission.Internal.Meta.Package
  ( module Fission.Internal.Meta.Package.Types
  , package
  , fromContents
  ) where

import Data.FileEmbed
import Data.Yaml as Yaml

import Fission.Prelude
import Fission.Internal.Meta.Package.Types

package :: Maybe Package
package = fromContents $(embedFile "./package.yaml")

fromContents :: ByteString -> Maybe Package
fromContents contents =
  case Yaml.decodeEither' contents of
    Left  _err -> Nothing
    Right val  -> Just val
