module Fission.Web.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import Network.HTTP.Types.Status
import Network.Wai.Internal (Request (..))
import Network.Wai.Logger

import Text.Pretty.Simple

import Fission.Prelude

rioApacheLogger ::
  MonadLogger m
  => Request
  -> Status
  -> Maybe Integer
  -> m ()
rioApacheLogger Request {..} Status {..} _mayInt =
  unless (statusCode == 404 && requestHeaderUserAgent == Just "ELB-HealthChecker/2.0") do
    if | statusCode >= 500 -> logError formatted
       | statusCode >= 400 -> logInfo  formatted
       | otherwise         -> logDebug formatted
  where
    formatted :: Utf8Builder
    formatted = mconcat
      [ displayShow remoteHost
      , " - - "
      , displayShow httpVersion
      , " - - "
      , display statusCode
      , ": "
      , displayShow statusMessage
      , " "
      , displayShow requestMethod
      , " "
      , if rawPathInfo    == "" then "" else displayShow rawPathInfo
      , " "
      , if rawQueryString == "" then "" else displayShow rawQueryString
      , " "
      , display $ pShow requestHeaders
      ]

fromLogFunc :: LogFunc -> ApacheLogger
fromLogFunc logger r s mi = runRIO logger $ rioApacheLogger r s mi
