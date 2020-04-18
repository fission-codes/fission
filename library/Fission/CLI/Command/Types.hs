module Fission.CLI.Command.Types
  ( Command    (..)
  -- , OptionInfo (..)
  ) where

import           Options.Applicative.Simple hiding (command)

import           Fission.Prelude

data Command m input output = Command
  { command     :: !Text
  , description :: !Text
  , parseArgs   :: !(Parser input)
  , handler     :: !(input -> m output)
  -- , subCommands  :: ![forall subInput . Command m subInput output]
  }

