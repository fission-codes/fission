-- | Helpers for working with a cursor in a console
module Fission.CLI.Display.Cursor (withHidden) where

import Fission.Prelude

import qualified System.Console.ANSI as ANSI

-- | Perform console actions without a cursor visible
withHidden :: MonadUnliftIO m => m a -> m a
withHidden action = bracket acquire release \_ -> action
  where
    acquire :: MonadIO m => m ()
    acquire = liftIO ANSI.hideCursor

    release :: MonadIO m => ignored -> m ()
    release _ = liftIO ANSI.showCursor
