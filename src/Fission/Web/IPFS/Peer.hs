{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Peer where

import RIO

import Servant
import Data.Has

import           Fission.Config
import qualified Fission.Internal.UTF8 as UTF8
import           Fission.IPFS.Peer     as Peer
import           Fission.Web.Internal

type API = Get '[JSON] [Peer]

server :: Has IpfsPath cfg => RIOServer cfg API
server = Peer.all >>= \case
  Left  unicodeErr -> throwM $ err500 { errBody = UTF8.showLazyBS unicodeErr }
  Right peers      -> return peers

api :: Proxy API
api = Proxy
