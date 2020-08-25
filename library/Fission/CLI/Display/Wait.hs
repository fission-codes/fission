-- | Wait for an action on the CLI
module Fission.CLI.Display.Wait (waitFor) where

import           Fission.Prelude
import           RIO.ByteString

import qualified System.Console.ANSI        as ANSI

import           Fission.CLI.Display.Loader
import           Fission.CLI.Display.Text

waitFor :: (MonadIO m, MonadCleanup m) => ByteString -> m a -> m a
waitFor msg action =
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
    liftIO $ ANSI.cursorForward 3
    putStr msg
    liftIO $ ANSI.setCursorColumn 0

    withLoader 5000 do
      action
