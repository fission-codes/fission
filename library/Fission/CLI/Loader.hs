-- | Visual flourishes for indicating a loading state
module Fission.CLI.Loader
  ( withLoader
  , reset
  , prep
  , putText
  , loading
  ) where

import           RIO
import           RIO.ByteString
import qualified RIO.List       as List

import           Control.Concurrent
import qualified System.Console.ANSI as ANSI

import qualified Fission.Emoji as Emoji

-- | Perform actions in the background while displaying a loading indicator
--
--   The indicator disappears when the process completes
withLoader :: Natural -> IO a -> IO a
withLoader delay = RIO.bracket (forkIO $ loading delay) cleanup . const
  where
    cleanup :: ThreadId -> IO ()
    cleanup pid = do
      killThread pid
      reset

-- | Reset the cursor position back one priontable character, and clear the *entire* line
reset :: IO ()
reset = do
  ANSI.cursorBackward 4
  ANSI.clearLine

-- | Prepare for the next step -- in this case wait and reset the line
prep :: MonadIO m => Natural -> m ()
prep delay = do
  RIO.threadDelay $ fromIntegral delay
  liftIO $ ANSI.cursorBackward 4

-- | Loading animation
loading :: MonadIO m => Natural -> m ()
loading delay = forever
        . (const (prep delay) <=< sequence_)
        . List.intersperse (prep delay)
        $ fmap putText
            [ Emoji.clock0100
            , Emoji.clock0200
            , Emoji.clock0300
            , Emoji.clock0400
            , Emoji.clock0500
            , Emoji.clock0600
            , Emoji.clock0700
            , Emoji.clock0800
            , Emoji.clock0900
            , Emoji.clock1000
            , Emoji.clock1100
            , Emoji.clock1200
            ]

-- | Helper for printing 'Text' to a console
putText :: MonadIO m => Text -> m ()
putText = putStr . encodeUtf8
