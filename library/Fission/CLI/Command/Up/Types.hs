module Fission.CLI.Command.Up.Types (Options(..)) where

import Fission.Prelude hiding (Options)

-- | Arguments, flags & switches for the `up` command
data Options = Options
  { dnsOnly :: Bool
  , path    :: FilePath
  }
