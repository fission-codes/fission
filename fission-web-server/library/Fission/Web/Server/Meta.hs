module Fission.Web.Server.Meta
  ( package
  , module Fission.Internal.Meta.Package
  ) where

import           Data.FileEmbed
import           Data.Yaml                           as Yaml

import           Fission.Internal.Meta.Package.Types

import           Fission.Prelude

package :: Maybe Package
package = fromContents $(embedFile "./package.yaml")

fromContents :: ByteString -> Maybe Package
fromContents contents =
  case Yaml.decodeEither' contents of
    Left  _err -> Nothing
    Right val  -> Just val
