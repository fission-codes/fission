module Fission.Internal.Log
 ( logInfo
 , logDebug
 , logDebugN
 , logWarn
 , logError
 , logErrorN
 , logOther
 ) where

import Control.Monad.Logger (ToLogStr, MonadLogger, LogLevel (..), logWithoutLoc, logDebugN, logErrorN)

logInfo :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logInfo = logWithoutLoc "" LevelInfo

logDebug :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logDebug = logWithoutLoc "" LevelDebug

logWarn :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logWarn = logWithoutLoc "" LevelWarn

logError :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logError = logWithoutLoc "" LevelError

logOther :: (ToLogStr msg, MonadLogger m) => LogLevel -> msg -> m ()
logOther = logWithoutLoc ""
