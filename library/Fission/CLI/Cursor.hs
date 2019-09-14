module Fission.CLI.Cursor (withHidden) where

import RIO

import qualified System.Console.ANSI as ANSI

withHidden :: MonadUnliftIO m => m c -> m c
withHidden = bracket (liftIO ANSI.hideCursor) (const $ liftIO ANSI.showCursor) . const
