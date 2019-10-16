-- | Wait for an action on the CLI
module Fission.CLI.Display.Wait (waitFor) where

import RIO
import RIO.ByteString

import qualified System.Console.ANSI as ANSI

import Fission.CLI.Display.Loader

waitFor :: MonadUnliftIO m => ByteString -> m a -> m a
waitFor msg action = do
  liftIO $ ANSI.cursorForward 3
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  putStr msg
  liftIO $ ANSI.setCursorColumn 0
  result <- withLoader 5000 action
  liftIO $ ANSI.setSGR [ANSI.Reset]
  return result
