module Fission.CLI.Command.Class (runWith) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer

import           Options.Applicative.Simple
import qualified RIO.Text as Text

import           Fission.Prelude
import           Fission.CLI.Command.Types

runWith ::
     (m output -> IO output)
  -> Command m input output
  -> ExceptT (IO output) (Writer (Mod CommandFields (IO output))) ()
runWith nt Command {..} =
  addCommand (Text.unpack command) (Text.unpack description) (nt . handler) argParser
