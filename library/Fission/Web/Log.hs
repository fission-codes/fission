module Fission.Web.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import RIO

import Servant.Server

import Network.HTTP.Types.Status
import Network.Wai.Internal (Request (..))
import Network.Wai.Logger

import Fission.Internal.Constraint

rioApacheLogger :: MonadRIO   cfg m
                => HasLogFunc cfg
                => MonadThrow     m
                => Request
                -> Status
                -> Maybe Integer
                -> m ()
rioApacheLogger request(Request {..}) Status {statusCode} _mayInt =
  if | statusCode >= 500 -> do
        logError formatted
        let err = toServerError statusCode { errBody = responseBody request }
       throwM $
     | statusCode >= 400 -> do
         logInfo  formatted

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

responseBody :: Response -> IO ByteString
responseBody response = body $ \f -> do
  content <- newIORef mempty
  f (\chunk -> modifyIORef' content (<> chunk)) (return ())
  toLazyByteString <$> readIORef content
  where
    (_, _, body) = responseToStream resposnse
