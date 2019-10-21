module Fission.CLI.Command.Up.Types (CommandOptions(..)) where

import RIO


-- | Arguments, flags & switches for the `up` command
data CommandOptions = CommandOptions
  { optDnsOnly :: Bool
  , optLocation :: String
  }
