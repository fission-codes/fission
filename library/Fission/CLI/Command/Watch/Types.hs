module Fission.CLI.Command.Watch.Types (Options(..)) where

import RIO

-- | Arguments, flags & switches for the `watch` command
data Options = Options
  { dnsOnly :: Bool
  , path    :: FilePath
  }
