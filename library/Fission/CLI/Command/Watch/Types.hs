module Fission.CLI.Command.Watch.Types (CommandOptions(..)) where

import RIO


-- | Arguments, flags & switches for the `watch` command
data CommandOptions = CommandOptions
  { optDnsOnly :: Bool
  , optLocation :: String
  }
