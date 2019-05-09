{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Upload where

import RIO

import Servant

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Upload   as Upload
import           Fission.Web.Internal

type API = ReqBody '[JSON, PlainText] Text
        :> Post    '[JSON, PlainText] Text

server :: FissionServer API
server input = Upload.run input >>= \case
  Left  unicodeErr ->
    throwM $ err500 { errBody = UTF8.showLazyBS unicodeErr }

  Right hash -> do
    logInfo $ "Generated hash: " <> display hash
    return hash

api :: Proxy API
api = Proxy
