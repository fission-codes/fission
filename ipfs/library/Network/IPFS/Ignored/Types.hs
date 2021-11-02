module Network.IPFS.Ignored.Types (Ignored) where

import qualified System.FilePath.Glob as Glob

type Ignored = [Glob.Pattern]
