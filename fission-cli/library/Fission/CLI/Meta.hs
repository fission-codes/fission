{-# LANGUAGE TemplateHaskell #-}

module Fission.CLI.Meta
  ( package
  , module Fission.Internal.Meta.Package
  ) where

import           Data.FileEmbed

import           Fission.Internal.Meta.Package hiding (package)
import           Fission.Prelude

package :: Maybe Package
package = fromContents $(embedFile "./package.yaml")
