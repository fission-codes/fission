{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Fission.Web.IPFS.Upload.Multipart
  ( API
  , server
  , api
  ) where

import RIO

import Data.Has

import Servant
import Servant.Multipart

import           Fission.Config
import qualified Fission.IPFS as IPFS
import           Fission.Web.Server

type API = MultipartForm Mem (MultipartData Mem)
        :> Post '[OctetStream, PlainText] IPFS.Address

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server form =
  case lookupFile "file" form of
    Just FileData { fdPayload } -> do
      hash <- IPFS.upload fdPayload
      logInfo $ "Generated address: " <> display hash
      return hash

    Nothing ->
      throwM $ err422 { errBody = "File not processable" }

api :: Proxy API
api = Proxy
