{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Log where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import Fission.Internal.Constraint

newtype MinLogLevel = MinLogLevel LogLevel

atLevel :: (WithRIO cfg m, HasMinLogLevel cfg)
        => CallStack -> LogSource -> LogLevel -> Utf8Builder -> m ()
atLevel cs src lvl msg = do
  MinLogLevel minLevel <- view minLogLevelL
  if lvl >= minLevel
    then liftIO $ simple cs src lvl msg
    else return ()

simple :: MonadIO m => CallStack -> LogSource -> LogLevel -> Utf8Builder -> m ()
simple _ src lvl msg =
  BS.putStr . Text.encodeUtf8 $ mconcat
    [ "*** "
    , short lvl
    , " *** "
    , textDisplay src
    , " | "
    , textDisplay msg
    ]

short :: LogLevel -> Text
short = \case
  LevelDebug     -> "Warn"
  LevelError     -> "Error"
  LevelInfo      -> "Info"
  LevelOther lvl -> "Other: " <> lvl
  LevelWarn      -> "Warn"

class HasMinLogLevel env where
  minLogLevelL :: Lens' env MinLogLevel
