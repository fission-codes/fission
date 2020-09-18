module Fission.Internal.CLI.Meta
  ( module Fission.Internal.Meta.Package
  , package
  ) where

import           Data.FileEmbed

import           Fission.Internal.Meta.Package hiding (package)
import           Fission.Prelude

package :: Maybe Package
package = fromContents $(embedFile "./package.yaml")
