module Fission.CLI.Parser.Watch.Types (WatchFlag (..)) where

import           Fission.Prelude

newtype WatchFlag = WatchFlag
  { unFlag :: Bool }
  deriving newtype (Show, Eq)
