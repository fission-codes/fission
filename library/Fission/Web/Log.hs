module Fission.Web.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import RIO

import Network.HTTP.Types.Status
import Network.Wai.Internal (Request (..))
import Network.Wai.Logger

import Fission.Internal.Constraint

rioApacheLogger
  :: MonadRIO   cfg m
  => HasLogFunc cfg
  => Request
  -> Status
  -> Maybe Integer
  -> m ()
rioApacheLogger Request {..} Status {statusCode} _mayInt =
  if | statusCode >= 500 -> logError formatted
     | statusCode >= 400 -> logInfo  formatted
     | otherwise         -> logDebug formatted
  where
    formatted :: Utf8Builder
    formatted = mconcat
      [ displayShow remoteHost
      , " - - "
      , displayShow httpVersion
      , " "
      , displayShow requestMethod
      , " "
      , if rawPathInfo    == "" then "" else displayShow rawPathInfo
      , if rawQueryString == "" then "" else displayShow rawQueryString
      , " "
      , displayShow statusCode
      , displayShow $ maybe "" (" - " <>) requestHeaderUserAgent
      ]

fromLogFunc :: LogFunc -> ApacheLogger
fromLogFunc logger r s mi = runRIO logger (rioApacheLogger r s mi)
