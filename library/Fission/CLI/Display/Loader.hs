-- | Visual flourishes for indicating a loading state
module Fission.CLI.Display.Loader
  ( withLoader
  , reset
  , prep
  , loading
  ) where

import           RIO
import qualified RIO.List as List

import           Control.Concurrent
import qualified System.Console.ANSI as ANSI

import qualified Fission.Internal.UTF8 as UTF8

-- | Perform actions in the background while displaying a loading indicator
--
--   The indicator disappears when the process completes
withLoader :: MonadUnliftIO m => Natural -> m a -> m a
withLoader delay action = RIO.bracket acquire release $ \_ -> action
  where
    acquire :: MonadIO m => m ThreadId
    acquire = liftIO . forkIO $ loading delay

    release :: MonadIO m => ThreadId -> m ()
    release pid = liftIO do
      killThread pid
      reset

-- | Reset the cursor position back one priontable character, and clear the *entire* line
reset :: MonadIO m => m ()
reset = liftIO do
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
        $ fmap UTF8.putText
            ["🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛"]
