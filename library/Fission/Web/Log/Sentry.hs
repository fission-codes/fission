-- | Error reporting via @sentry.io@
module Fission.Web.Log.Sentry
  ( onException
  , mkInnerLogger
  ) where

import           RIO hiding (onException)
import qualified RIO.List as List
import qualified RIO.Text as Text
import           RIO.Time (UTCTime, getCurrentTime)

import Data.ByteString.Char8

import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server

import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit
import System.Log.Raven.Types as Sentry

import qualified Fission.Web.Log.Sentry.DSN.Types as Sentry

mkInnerLogger :: MonadUnliftIO m => LogLevel -> Sentry.DSN -> m LogFunc
mkInnerLogger minRIOLogLevel (Sentry.DSN dsn) = do
  raven <- liftIO $ initRaven dsn id sendRecord silentFallback
  return $ mkLogFunc $ innerLogger minRIOLogLevel raven

-- | Log from inside the application
innerLogger
  :: MonadUnliftIO m
  => LogLevel
  -> SentryService
  -> CallStack
  -> LogSource
  -> LogLevel
  -> Utf8Builder
  -> m ()
innerLogger minRIOLogLevel sentryService _cs logSource logLevel msg =
  liftIO $ when (minRIOLogLevel >= logLevel) do
    timestamp <- getCurrentTime
    register sentryService loggerName level (message timestamp) (sentryRecord timestamp)
  where
    level :: SentryLevel
    level = fromRIOLogLevel logLevel

    message :: UTCTime -> String
    message time = mconcat $ List.intersperse " --- "
      [ "Time: "    <> show time
      , "Level: "   <> show logLevel
      , "Source: "  <> Text.unpack logSource
      , "Message: " <> Text.unpack (textDisplay msg)
      ]

    sentryRecord :: UTCTime -> SentryRecord -> SentryRecord
    sentryRecord time oldRecord =
      oldRecord
        { srCulprit = Just "Internal Logger"
        , srMessage = message time
        , srLevel   = level
        }

-- | Exception handler to be used as a setting for WAI middleware
onException :: Sentry.DSN -> Maybe Request -> SomeException -> IO ()
onException (Sentry.DSN dsn) mayRequest exception = do
  sentryService <- initRaven dsn id sendRecord silentFallback
  register sentryService loggerName (toErrLevel exception) message sentryRecord
  defaultOnException mayRequest exception
  where
    message :: String
    message = formatMessage exception mayRequest

    sentryRecord :: SentryRecord -> SentryRecord
    sentryRecord = recordUpdate mayRequest exception

formatMessage :: SomeException -> Maybe Request -> String
formatMessage exception = \case
  Nothing      -> "Exception before request could be parsed: " <> show exception
  Just request -> "Exception " <> show exception <> " while handling request " <> show request

-- | The name to report this logger as to Sentry
loggerName :: String
loggerName = "Fission.Web.Log.Sentry"


recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing        _ record' = record'
recordUpdate (Just request) _ record' =
  record'
    { srCulprit    = Just $ unpack $ rawPathInfo request
    , srServerName = unpack <$> requestHeaderHost request
    }

toErrLevel :: SomeException -> SentryLevel
toErrLevel err = case fromException err of
  Just (ServerError { errHTTPCode }) ->
    if | errHTTPCode <= 500 -> Sentry.Error
       | errHTTPCode <= 400 -> Sentry.Warning
       | otherwise          -> Sentry.Info

  _ -> Sentry.Error

fromRIOLogLevel :: LogLevel -> SentryLevel
fromRIOLogLevel = \case
  LevelWarn      -> Sentry.Warning
  LevelError     -> Sentry.Error
  LevelOther msg -> Sentry.Custom $ Text.unpack msg
  _              -> Sentry.Info
