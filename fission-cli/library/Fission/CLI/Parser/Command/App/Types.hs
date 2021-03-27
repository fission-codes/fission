module Fission.CLI.Parser.Command.App.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.App.Info.Types as Info
import qualified Fission.CLI.Parser.Command.App.Init.Types as Init
import qualified Fission.CLI.Parser.Command.App.Up.Types   as Up

data Options
  = Init Init.Options
  | Info Info.Options
  | Up   Up.Options
  deriving (Show, Eq)
