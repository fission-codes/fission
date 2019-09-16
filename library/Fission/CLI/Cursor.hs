-- | Helpers for working with a cursor in a console
module Fission.CLI.Cursor (withHidden) where

import RIO

import qualified System.Console.ANSI as ANSI

-- | Perform console actions without a cursor visible
withHidden :: MonadUnliftIO m => m c -> m c
withHidden = bracket (liftIO ANSI.hideCursor) (const $ liftIO ANSI.showCursor) . const
