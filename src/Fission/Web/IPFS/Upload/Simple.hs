{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeOperators         #-}

module Fission.Web.IPFS.Upload.Simple
  ( API
  , server
  , api
  ) where

import RIO

import Data.Has
import Servant

import Fission.Config
import Fission.Web.Server

import qualified Fission.File as File
import qualified Fission.IPFS as IPFS

type API = ReqBody '[OctetStream, PlainText] File.Serialized
        :> Post    '[OctetStream, PlainText] IPFS.Address

server :: Has IpfsPath cfg => RIOServer cfg API
server = IPFS.upload . File.unserialize

api :: Proxy API
api = Proxy
