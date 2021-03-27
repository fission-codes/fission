module Fission.Internal.Log
 ( logInfo
 , logDebug
 , logDebugN
 , logWarn
 , logError
 , logErrorN
 , logOther
 , logUser
 ) where

import           Control.Monad.Logger hiding (logDebug, logError, logInfo,
                                       logOther, logWarn)
import           GHC.Stack
import           RIO                  (Text, decodeUtf8Lenient, ($), (.))

logInfo :: (HasCallStack, ToLogStr msg, MonadLogger m) => msg -> m ()
logInfo msg = logInfoCS callStack $ format msg

logDebug :: (HasCallStack, ToLogStr msg, MonadLogger m) => msg -> m ()
logDebug msg = logDebugCS callStack $ format msg

logWarn :: (HasCallStack, ToLogStr msg, MonadLogger m) => msg -> m ()
logWarn msg = logWarnCS callStack $ format msg

logError :: (HasCallStack, ToLogStr msg, MonadLogger m) => msg -> m ()
logError msg = logErrorCS callStack $ format msg

logUser :: (HasCallStack, ToLogStr msg, MonadLogger m) => msg -> m ()
logUser msg = logOther "user" $ format msg

logOther :: (HasCallStack, ToLogStr msg, MonadLogger m) => Text -> msg -> m ()
logOther lvlTxt msg = logOtherCS callStack (LevelOther lvlTxt) (format msg)

format :: ToLogStr msg => msg -> Text
format msg = decodeUtf8Lenient . fromLogStr $ toLogStr msg
