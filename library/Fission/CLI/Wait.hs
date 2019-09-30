-- | Wait for an action on the CLI
module Fission.CLI.Wait (waitFor) where

import RIO
import RIO.ByteString

import qualified System.Console.ANSI as ANSI

import Fission.CLI.Loader

waitFor :: ByteString -> IO a -> IO a
waitFor msg action = do
  ANSI.cursorForward 3
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  putStr msg
  ANSI.setCursorColumn 0
  result <- withLoader 5000 action
  ANSI.setSGR [ANSI.Reset]
  return result
