module Fission.CLI.Command.Watch.Types (Options(..)) where

import Fission.Prelude hiding (Options)

-- | Arguments, flags & switches for the `watch` command
data Options = Options
  { dnsOnly :: Bool
  , path    :: FilePath
  }
