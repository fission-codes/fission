module Fission.IPFS.Ignored.Types (Ignored (..)) where

import           Fission.Prelude
import qualified System.FilePath.Glob as Glob

type Ignored = [Glob.Pattern]
