-- | Error reporting via @sentry.io@
module Fission.Web.Log.Sentry
  ( mkLogger
  , logger
  , fromRIOLogLevel
  ) where

import           RIO hiding (onException)
import qualified RIO.List as List
import qualified RIO.Text as Text
import           RIO.Time (UTCTime, getCurrentTime)

import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit
import System.Log.Raven.Types as Sentry

import qualified Fission.Web.Log.Sentry.DSN.Types as Sentry

-- | Instantiate a Sentry logger
mkLogger :: MonadUnliftIO m => LogLevel -> Sentry.DSN -> m LogFunc
mkLogger minRIOLogLevel (Sentry.DSN dsn) = do
  raven <- liftIO $ initRaven dsn id sendRecord silentFallback
  return . mkLogFunc $ logger minRIOLogLevel raven

-- | Log from inside the application
logger
  :: MonadUnliftIO m
  => LogLevel
  -> SentryService
  -> CallStack
  -> LogSource
  -> LogLevel
  -> Utf8Builder
  -> m ()
logger minRIOLogLevel sentryService _cs logSource logLevel msg =
  liftIO $ when (logLevel >= minRIOLogLevel) do
    timestamp <- getCurrentTime
    register sentryService loggerName level (message timestamp) (sentryRecord timestamp)
  where
    level :: SentryLevel
    level = fromRIOLogLevel logLevel

    message :: UTCTime -> String
    message time = mconcat $ List.intersperse " - - "
      [ Text.unpack (textDisplay msg)
      , show time
      , show logLevel
      , if logSource == mempty then "" else Text.unpack logSource
      ]

    sentryRecord :: UTCTime -> SentryRecord -> SentryRecord
    sentryRecord time oldRecord =
      oldRecord
        { srMessage = message time
        , srLevel   = level
        }

-- | The name to report this logger as to Sentry
loggerName :: String
loggerName = "Fission.Web.Log.Sentry"

fromRIOLogLevel :: LogLevel -> SentryLevel
fromRIOLogLevel = \case
  LevelWarn      -> Sentry.Warning
  LevelError     -> Sentry.Error
  LevelOther msg -> Sentry.Custom $ Text.unpack msg
  _              -> Sentry.Info
