{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Network.Wai
import Servant

import Fission.Env
import Fission.Web.Types

import           Fission.IPFS.Peer   as Peer
import qualified Fission.IPFS.Upload as Upload

type UploadAPI = ReqBody '[JSON, PlainText] Text
                   :> Post '[JSON, PlainText] Text

type API = UploadAPI
      :<|> "peers" :> Get '[JSON] [Peer]

toServer :: Env -> Server API
toServer env = hoistServer api (toHandler env) serverT

app :: Env -> Application
app = serve api . toServer

serverT :: FissionServer API
serverT = Upload.test :<|> Peer.all

api :: Proxy API
api = Proxy

-- -- curl -v -H "Content-Type: text/plain;charset=utf-8" -H "charset=ascii" -H "Accept: text/plain" localhost:8000/ipfs/echo -d "{'fdsa': 1}"
-- uploader :: Text -> Servant.Handler Text
-- uploader text = return Upload.insert text
