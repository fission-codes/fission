-- | Application logging
module Fission.Log
  ( MinLevel (..)
  , atLevel
  , short
  , simple
  ) where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import Data.Has

import qualified Fission.Config as Config
import           Fission.Internal.Constraint
import           Fission.Log.Types

-- | Filter for log message output to a certain level
atLevel :: MonadRIO     cfg m
        => Has MinLevel cfg
        => CallStack
        -> LogSource
        -> LogLevel
        -> Utf8Builder
        -> m ()
atLevel cs src lvl msg = do
  MinLevel minLevel <- Config.get
  when (lvl >= minLevel) $
    liftIO $ simple cs src lvl msg

-- | A simple logger format
--
--   Includes a newline
--
--   === Example
--
--   >>> simple callStack "app" LevelDebug "This is a log message"
--   *** Debug ***  | This is a log message
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

-- | Short logger format
--
--   === Example
--
--   >>> short callStack "app" LevelDebug "This is a log message"
--   Debug: This is a log message
short :: LogLevel -> Text
short = \case
  LevelDebug     -> "Debug"
  LevelError     -> "Error"
  LevelInfo      -> "Info"
  LevelWarn      -> "Warn"
  LevelOther lvl -> "Other: " <> lvl
