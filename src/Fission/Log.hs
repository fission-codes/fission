module Fission.Log
  ( MinLogLevel (..)
  , atLevel
  , short
  , simple
  ) where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import Data.Has

import Fission.Internal.Constraint

newtype MinLogLevel = MinLogLevel LogLevel

atLevel :: MonadRIO cfg m
        => Has MinLogLevel cfg
        => CallStack -> LogSource -> LogLevel -> Utf8Builder -> m ()
atLevel cs src lvl msg = do
  MinLogLevel minLevel <- view hasLens
  when (lvl >= minLevel) $
    liftIO $ simple cs src lvl msg

simple :: MonadIO m => CallStack -> LogSource -> LogLevel -> Utf8Builder -> m ()
simple _ src lvl msg =
  BS.putStr . Text.encodeUtf8 $ mconcat
    [ "*** "
    , short lvl
    , " *** "
    , textDisplay src
    , " | "
    , textDisplay msg
    , "\n"
    ]

short :: LogLevel -> Text
short = \case
  LevelDebug     -> "Warn"
  LevelError     -> "Error"
  LevelInfo      -> "Info"
  LevelWarn      -> "Warn"
  LevelOther lvl -> "Other: " <> lvl
