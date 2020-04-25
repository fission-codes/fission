module Fission.Web.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import Network.HTTP.Types.Status
import Network.Wai.Internal (Request (..))
import Network.Wai.Logger

import Fission.Prelude

rioApacheLogger ::
  MonadLogger m
  => Request
  -> Status
  -> Maybe Integer
  -> m ()
rioApacheLogger Request {..} Status {..} _mayInt =
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
      , displayShow statusCode
      , ": "
      , displayShow statusMessage
      , " "
      , displayShow requestMethod
      , " "
      , if rawPathInfo    == "" then "" else displayShow rawPathInfo
      , " "
      , if rawQueryString == "" then "" else displayShow rawQueryString
      , " "
      , displayShow requestHeaders
      ]

fromLogFunc :: LogFunc -> ApacheLogger
fromLogFunc logger r s mi = runRIO logger $ rioApacheLogger r s mi
