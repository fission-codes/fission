module Fission.CLI.Environment.OS
  ( parse
  , get
  -- * Reexports
  , module Fission.CLI.Environment.OS.Error
  , module Fission.CLI.Environment.OS.Types
  ) where

import qualified System.Info                      as System

import           Fission.Prelude

import qualified Fission.CLI.Environment.OS.Error as OS
import qualified Fission.CLI.Environment.OS.Types as OS

-- Reexports

import           Fission.CLI.Environment.OS.Error
import           Fission.CLI.Environment.OS.Types

get :: Either OS.Unsupported OS.Supported
get = parse System.os

parse :: String -> Either OS.Unsupported OS.Supported
parse = \case
  "linux"  -> Right Linux
  "darwin" -> Right MacOS
  other    -> Left $ OS.Unsupported other
