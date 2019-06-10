module Fission.Web.IPFS.Upload.Multipart
  ( API
  , server
  ) where

import RIO

import Data.Has

import Servant
import Servant.Multipart

import           Fission.Config
import qualified Fission.IPFS.Address as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import           Fission.Web.Server

type API = MultipartForm Mem (MultipartData Mem)
        :> Post '[OctetStream, PlainText] IPFS.Address

server :: (Has IPFSPath cfg, HasLogFunc cfg) => RIOServer cfg API
server form =
  case lookupFile "file" form of
    Just FileData { fdPayload } -> do
      hash <- Storage.IPFS.add fdPayload
      logInfo $ "Generated address: " <> display hash
      return hash

    Nothing ->
      throwM $ err422 { errBody = "File not processable" }
