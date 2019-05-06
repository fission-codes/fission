{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Network.Wai
import Servant

import Fission.IPFS.Peer as Peer

type API = {- ROOT -} ReqBody '[JSON] Text :> Post '[JSON] Text
      :<|> "peers"                         :> Get  '[JSON] [Peer]


app :: Application
app = serve api server

server :: Server API
server = liftIO <$> foo :<|> Peer.all

api :: Proxy API
api = Proxy

-- foo :: UploadMe -> Hash
foo :: Text -> IO Text
foo toUpload = return toUpload
