{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Upload where

import RIO

import Servant

import qualified Fission.IPFS.Upload  as Upload
import           Fission.Web.Internal

type API = ReqBody '[JSON, PlainText] Text
        :> Post    '[JSON, PlainText] Text

server :: FissionServer API
server = Upload.test

api :: Proxy API
api = Proxy
