{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Fission.Web.IPFS where

import RIO

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Aeson
import Data.Aeson.TH

import Network.Wai
import Servant

import System.Process.Typed

data Peer = Peer { peer :: Text }
$(deriveJSON defaultOptions ''Peer)

type API = "peers" :> Get '[JSON] [Peer]

ipfsApp :: Application
ipfsApp = serve api server

getPeersRaw :: MonadIO m => m Lazy.ByteString
getPeersRaw =
  readProcessStdout_
  . setStdin createPipe
  . setStdout byteStringOutput
  $ proc "/usr/local/bin/ipfs" ["bootstrap", "list"]

toText :: Lazy.ByteString -> Either UnicodeException Text
toText = decodeUtf8' . Lazy.toStrict

getPeers :: MonadIO m => m [Peer] -- (Either UnicodeException Text)
getPeers =
  getPeersRaw >>= \raw ->
    case (toText raw) of
        Left err   -> error $ show err
        Right text -> return $ Peer <$> Text.lines text

server :: Server API
server = liftIO getPeers

api :: Proxy API
api = Proxy
