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
rioApacheLogger request@Request {..} Status {statusCode} _mayInt =
  if | statusCode >= 500 -> do
        logError formatted
        -- body <- responseBody request
        throwM $ (toServerError statusCode) -- { errBody = body }

     | statusCode >= 400 -> do
         logInfo  formatted
         -- body <- responseBody request
         throwM $ (toServerError statusCode) -- { errBody = body }

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

-- responseBody :: Request -> IO ByteString
-- responseBody response = body $ \f -> do
--   content <- newIORef mempty
--   f (\chunk -> modifyIORef' content (<> chunk)) (return ())
--   toLazyByteString <$> readIORef content
--   where
--     (_, _, body) = responseToStream response
