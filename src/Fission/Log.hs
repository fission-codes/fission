{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Log where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

type Logger = CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()

simple :: Logger
simple _ src _ msg =
  BS.putStr $ Text.encodeUtf8 $ mconcat
    [ "***"
    -- , textDisplay lvl
    , "*** "
    , textDisplay src
    , " | "
    , textDisplay msg
    ]
