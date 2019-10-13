module Fission.CLI.Error (cliLog) where

import RIO

import Fission.Internal.Constraint

import           Fission.CLI.Error.Types
import qualified Fission.CLI.Display.Error as CLI.Error

cliLog :: (MonadRIO cfg m, HasLogFunc cfg) => Error -> m ()
cliLog = CLI.Error.put'
