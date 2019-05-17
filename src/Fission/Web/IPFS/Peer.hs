{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Peer
  ( API
  , server
  ) where

import RIO

import Servant
import Data.Has

import Fission.Config
import Fission.Web.Server

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Peer     as IPFS
import qualified Fission.IPFS.Peer     as IPFS.Peer

type API = Get '[JSON] [IPFS.Peer]

server :: Has IpfsPath cfg => RIOServer cfg API
server = IPFS.Peer.all >>= \case
  Left  unicodeErr -> throwM $ err500 { errBody = UTF8.showLazyBS unicodeErr }
  Right peers      -> return peers
