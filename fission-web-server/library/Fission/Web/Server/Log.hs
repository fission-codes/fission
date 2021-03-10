module Fission.Web.Server.Log
  ( rioApacheLogger
  , fromLogFunc
  ) where

import qualified Data.CaseInsensitive      as Case

import qualified RIO.ByteString.Lazy       as Lazy
import qualified RIO.List                  as List
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
      mconcat $ List.intersperse " "
        [ displayShow remoteHost
        , "- -"
        , displayShow httpVersion
        , display statusCode
        , displayBytesUtf8 requestMethod
        , pathInfo'
        , displayBytesUtf8 headers'
        , displayBytesUtf8 statusMessage
        ]

    headers' =
      requestHeaders
        |> fmap (\(k, v) -> (decodeUtf8Lenient (Case.original k), decodeUtf8Lenient v))
        |> Map.fromList
        |> encode
        |> Lazy.toStrict

    pathInfo' =
      if rawPathInfo == ""
        then ""
        else displayBytesUtf8 rawPathInfo <> queryString'

    queryString' =
      if rawQueryString == ""
        then ""
        else displayBytesUtf8 rawQueryString -- NOTE includes the `?`
