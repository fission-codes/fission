module Fission.CLI.Command
  ( runWith
  , module Fission.CLI.Command.Types
  ) where

import           Options.Applicative.Simple hiding (command)
import qualified RIO.Text                   as Text

import           Fission.Prelude

import           Fission.CLI.Command.Types
import           Fission.CLI.Command.Types  as Command

runWith :: (m () -> IO ()) -> Command m input () -> Command.Leaf
runWith nt Command {..} =
  addCommand
    (Text.unpack command)
    (Text.unpack description)
    (nt . handler)
    argParser
