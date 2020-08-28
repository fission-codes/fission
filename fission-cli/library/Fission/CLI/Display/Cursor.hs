-- | Helpers for working with a cursor in a console
module Fission.CLI.Display.Cursor (withHidden) where

import           Fission.Prelude

import qualified System.Console.ANSI as ANSI

-- | Perform console actions without a cursor visible
withHidden :: (MonadIO m, MonadCleanup m) => m a -> m a
withHidden action = do
  liftIO ANSI.hideCursor
  action `always` liftIO ANSI.showCursor
