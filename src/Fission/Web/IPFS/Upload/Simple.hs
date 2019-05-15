{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeOperators         #-}

module Fission.Web.IPFS.Upload.Simple where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Servant

import Data.Has

import Fission.Config
import Fission.Web.Server

import qualified Fission.File as File
import qualified Fission.IPFS as IPFS

type API = ReqBody '[OctetStream, PlainText] File.Serialized
        :> Post    '[OctetStream, PlainText] IPFS.Address

newtype RawFile = RawFile { unUTF8 :: Lazy.ByteString }

instance MimeUnrender PlainText RawFile where
  mimeUnrender _proxy = Right . RawFile

instance MimeUnrender OctetStream RawFile where
  mimeUnrender _proxy = Right . RawFile

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = IPFS.upload . File.unserialize

api :: Proxy API
api = Proxy
