{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Network.Wai
import Servant

import Fission.IPFS.Peer   as Peer
import Fission.IPFS.Upload as Upload

type API = ReqBody '[JSON, PlainText] Text
             :> Post '[JSON, PlainText] Text
      :<|> "peers"
             :> Get  '[JSON] [Peer]

app :: Application
app = serve api server

server :: Server API
server = liftIO <$> (\txt -> Upload.test txt) :<|> Peer.all

api :: Proxy API
api = Proxy

-- -- curl -v -H "Content-Type: text/plain;charset=utf-8" -H "charset=ascii" -H "Accept: text/plain" localhost:8000/ipfs/echo -d "{'fdsa': 1}"
-- uploader :: Text -> Servant.Handler Text
-- uploader text = return Upload.insert text
