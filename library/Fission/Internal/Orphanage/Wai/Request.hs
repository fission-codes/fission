{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Wai.Request where

import RIO

import Network.Wai.Internal
import Data.Aeson

instance ToJSON Request where
  toJSON Request { .. } = object
    [ "requestMethod"          .= String (tshow requestMethod)
    , "httpVersion"            .= String (tshow httpVersion)
    , "rawPathInfo"            .= String (tshow rawPathInfo)
    , "rawQueryString"         .= String (tshow rawQueryString)
    , "requestHeaders"         .= toJSON (String . tshow <$> requestHeaders)
    , "isSecure"               .= toJSON isSecure
    , "remoteHost"             .= String (tshow remoteHost)
    , "pathInfo"               .= toJSON pathInfo
    , "requestBodyLength"      .= String (tshow requestBodyLength)
    , "requestHeaderHost"      .= String (tshow requestHeaderHost)
    , "requestHeaderRange"     .= String (tshow requestHeaderRange)
    , "requestHeaderReferer"   .= String (tshow requestHeaderReferer)
    , "requestHeaderUserAgent" .= toJSON (fmap tshow requestHeaderUserAgent)
    ]
