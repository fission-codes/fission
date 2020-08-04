module Fission.CLI.Parser.Command.App.Types (Options (..)) where

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.Parser.Command.App.Info.Types as Info
import qualified Fission.CLI.Parser.Command.App.Init.Types as Init
import qualified Fission.CLI.Parser.Command.App.Up.Types   as Up

data Options
  = Init Init.Options
  | Info Info.Options
  | Up   Up.Options
  deriving (Show, Eq)

instance Has VerboseFlag Options where
  getter = \case
    Init init -> getter init
    Info info -> getter info
    Up   up   -> getter up

  modifier run = \case
    Init init -> Init $ modifier run init
    Info info -> Info $ modifier run info
    Up   up   -> Up   $ modifier run up
