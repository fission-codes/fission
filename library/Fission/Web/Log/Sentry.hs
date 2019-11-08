-- | Error reporting via @sentry.io@
module Fission.Web.Log.Sentry (onException) where

import RIO hiding (onException)

import Data.Aeson.Encode.Pretty
import Data.ByteString.Char8

import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server

import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit
import System.Log.Raven.Types as Sentry

import           Fission.Web.Log.RequestErr.Types
import qualified Fission.Web.Log.Sentry.Types as Sentry

-- | Exception handler to be used as a setting for WAI middleware
onException :: Sentry.DSN -> Maybe Request -> SomeException -> IO ()
onException (Sentry.DSN dsn) mayRequest exception = do
  sentryService <- initRaven dsn id sendRecord silentFallback
  register sentryService loggerName (toErrLevel exception) message sentryRecord
  defaultOnException mayRequest exception
  where
    message :: String
    message = show . encodePretty $ RequestErr mayRequest exception

    sentryRecord :: SentryRecord -> SentryRecord
    sentryRecord = recordUpdate mayRequest exception

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
