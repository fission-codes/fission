module Fission.CLI.Command.Up.Types (Options(..)) where

import RIO

-- | Arguments, flags & switches for the `up` command
data Options = Options
  { dnsOnly :: Bool
  , path    :: FilePath
  }
