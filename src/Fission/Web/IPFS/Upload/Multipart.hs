module Fission.Web.IPFS.Upload.Multipart
  ( API
  -- , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant
import Servant.Multipart

import           Fission.Web.Server
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API = TextAPI :<|> JSONAPI

type TextAPI = FileRequest
               :> NameQuery
               :> Post '[OctetStream, PlainText] IPFS.Path

type JSONAPI = FileRequest
               :> NameQuery
               :> Post '[JSON] IPFS.SparseTree

type FileRequest = MultipartForm Mem (MultipartData Mem)
type NameQuery   = QueryParam "name" (Maybe String)

textAdd :: Has IPFS.Path     cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg TextAPI
textAdd form = run \case
  Directory [outer] (Dir [(name, inner)]) -> outer <> "/" <> name
  _ -> throwM $ err500 { errBody = "IPFS add error" }

-- jsonAdd :: Has IPFS.Path     cfg
--         => HasProcessContext cfg
--         => HasLogFunc        cfg
--         => RIOServer         cfg JSONAPI
-- jsonAdd form = run \case
--   Root outer (Dir [(name, inner)]) -> outer <> "/" <> name
--   _ -> throwM $ err500 { errBody = "IPFS add error" }

-- run :: String
run form cont =
  case lookupFile "file" form of
    Just FileData { fdPayload, fdFileName, fdInputName } -> do
      structure <- Storage.IPFS.addFile fdPayload -- (Text.unpack (fdInputName OR fdFileName))
      case structure of
        Left _ -> throwM $ err500 { errBody = "IPFS add error" }
        Right structure -> cont structure

    Nothing ->
      throwM $ err422 { errBody = "File not processable" }
