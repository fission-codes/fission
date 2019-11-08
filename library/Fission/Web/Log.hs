-- | Web logging helpers and middleware
module Fission.Web.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import RIO

import Network.HTTP.Types.Status
import Network.Wai.Internal (Request (..))
import Network.Wai.Logger

import Fission.Internal.Constraint
import Fission.Web.Error

fromLogFunc :: LogFunc -> ApacheLogger
fromLogFunc logger request status fileSize =
  runRIO logger (rioApacheLogger request status fileSize)

rioApacheLogger
  :: ( MonadRIO   cfg m
     , MonadThrow     m
     , HasLogFunc cfg
     )
   => Request
   -> Status
   -> Maybe Integer
   -> m ()
rioApacheLogger request@Request {..} Status {statusCode} _mayLogSize =
  if | statusCode >= 500 -> do
        logError formatted
        throwM $ (toServerError statusCode)

     | statusCode >= 400 -> do
         logInfo  formatted
         throwM $ (toServerError statusCode)

     | otherwise ->
         logDebug formatted
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
