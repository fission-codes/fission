module Fission.Web.Server.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import qualified RIO.ByteString.Lazy       as Lazy
import qualified RIO.Map                   as Map

import           Network.HTTP.Types.Status
import           Network.Wai.Internal      (Request (..))
import           Network.Wai.Logger

import           Fission.Prelude

fromLogFunc :: LogFunc -> ApacheLogger
fromLogFunc logger r s mi = runRIO logger $ rioApacheLogger r s mi

rioApacheLogger ::
  MonadLogger m
  => Request
  -> Status
  -> Maybe Integer
  -> m ()
rioApacheLogger Request {..} Status {..} _mayInt =
  unless (requestHeaderUserAgent == Just "ELB-HealthChecker/2.0") do
    if | statusCode >= 500 -> logError formatted
       | statusCode == 404 -> logInfo  formatted
       | statusCode >= 400 -> logWarn  formatted
       | otherwise         -> logDebug formatted
  where
    formatted :: Utf8Builder
    formatted =
      mconcat
        [ "[client "      <> displayShow remoteHost          <> "] "
        , "[httpVersion " <> displayShow httpVersion         <> "] "
        , "[status "      <> display statusCode              <> "] "
        , "[method "      <> displayBytesUtf8 requestMethod  <> "] "
        , pathInfo
        , "[headers "     <> displayBytesUtf8 headers'       <> "] "
        , "[message "     <> displayBytesUtf8 statusMessage' <> "] "
        ]

    headers' =
      requestHeaders
        |> fmap (\(k, v) -> (show k, show v))
        |> Map.fromList
        |> encode
        |> Lazy.toStrict

    pathInfo' =
      if rawPathInfo == ""
        then ""
        else "[path " <> displayBytesUtf8 rawPathInfo <> queryString' <> "]"

    queryString' =
      if rawQueryString == ""
        then ""
        else displayBytesUtf8 rawQueryString -- NOTE includes the `?`
