{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Fission.Web.IPFS.Upload
  ( API
  , server
  , api
  ) where

import RIO

import Data.Has
import Servant

import           Fission.Config
import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import           Fission.Web.Server

type API = Simple.API :<|> Multipart.API

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Simple.server :<|> Multipart.server

api :: Proxy API
api = Proxy
