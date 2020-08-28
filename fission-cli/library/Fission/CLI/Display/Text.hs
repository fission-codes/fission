module Fission.CLI.Display.Text (colourized) where

import qualified System.Console.ANSI as ANSI

import           Fission.Prelude

colourized :: (MonadIO m, MonadCleanup m) => [ANSI.SGR] -> m a -> m a
colourized colours actions =
  do
    liftIO $ ANSI.setSGR colours
    actions
  `always`
    liftIO (ANSI.setSGR [ANSI.Reset])
