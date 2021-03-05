-- | Error reporting via @sentry.io@
module Fission.Web.Server.Sentry
  ( mkLogger
  , logger
  , fromRIOLogLevel
  ) where

import qualified RIO
import qualified RIO.Text                               as Text

import           Data.Version                           (showVersion)

import           System.Log.Raven
import           System.Log.Raven.Transport.HttpConduit
import           System.Log.Raven.Types                 as Sentry

import qualified Servant.Client.Core                    as Servant

import           Fission.Prelude                        hiding (onException)

import           Fission.Web.Server.Host.Types
import qualified Fission.Web.Server.Sentry.DSN.Types    as Sentry

import qualified Paths_fission_web_server               as Fission

-- | Instantiate a Sentry logger
mkLogger :: MonadUnliftIO m => Host -> RIO.LogLevel -> Sentry.DSN -> m LogFunc
mkLogger host minRIOLogLevel (Sentry.DSN dsn) = do
  raven <- liftIO $ initRaven dsn identity sendRecord silentFallback
  raven
    |> logger host minRIOLogLevel
    |> mkLogFunc
    |> pure

-- | Log from inside the application
logger
  :: MonadUnliftIO m
  => Host
  -> RIO.LogLevel
  -> SentryService
  -> CallStack
  -> LogSource
  -> RIO.LogLevel
  -> Utf8Builder
  -> m ()
logger (Host host) minRIOLogLevel sentryService _cs _logSource logLevel msg =
  liftIO $ when (logLevel >= minRIOLogLevel) do
    now <- getCurrentTime
    register sentryService loggerName level message (sentryRecord now)
  where
    level :: SentryLevel
    level = fromRIOLogLevel logLevel

    message :: String
    message = Text.unpack $ utf8BuilderToText msg

    sentryRecord :: UTCTime -> SentryRecord -> SentryRecord
    sentryRecord time oldRecord =
      oldRecord
        { srMessage    = message
        , srLevel      = level
        , srTimestamp  = time
        , srPlatform   = Just "Haskell/Servant"
        , srServerName = Just $ Servant.baseUrlHost host
        , srRelease    = Just $ showVersion Fission.version
        }

-- | The name to report this logger as to Sentry
loggerName :: String
loggerName = "Fission.Web.Log.Sentry"

fromRIOLogLevel :: RIO.LogLevel -> SentryLevel
fromRIOLogLevel = \case
  RIO.LevelWarn      -> Sentry.Warning
  RIO.LevelError     -> Sentry.Error
  RIO.LevelOther msg -> Sentry.Custom $ Text.unpack msg
  _                  -> Sentry.Info
