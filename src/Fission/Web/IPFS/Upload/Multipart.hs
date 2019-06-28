module Fission.Web.IPFS.Upload.Multipart
  ( API
  -- , add
  ) where

import RIO
import RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Data.Has
import Servant
import Servant.Multipart

import           Fission.Web.Server
import           Fission.Internal.Constraint
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.IPFS          as Storage.IPFS
import qualified Fission.IPFS.Error            as IPFS.Error
import qualified Fission.Web.IPFS.Upload.Error as IPFS

type API = TextAPI :<|> JSONAPI

type TextAPI = FileRequest
               :> NameQuery
               :> Post '[OctetStream, PlainText] IPFS.Path

type JSONAPI = FileRequest
               :> NameQuery
               :> Post '[JSON] IPFS.SparseTree

type FileRequest = MultipartForm Mem (MultipartData Mem)
type NameQuery   = QueryParam "name" IPFS.Name

textAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg TextAPI
textAdd form = run form (either IPFS.throwErr pure . IPFS.linearize)

-- jsonAdd :: Has IPFS.Path     cfg
--         => HasProcessContext cfg
--         => HasLogFunc        cfg
--         => RIOServer         cfg JSONAPI
-- jsonAdd form = run \case
--   Root outer (Dir [(name, inner)]) -> outer <> "/" <> name
--   _ -> throwM $ err500 { errBody = "IPFS add error" }

run :: MonadRIO          cfg m
    => MonadThrow            m
    => Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => MultipartData Mem
    -> Maybe IPFS.Name
    -> (IPFS.SparseTree -> m a)
    -> m a
run form queryName cont = case lookupFile "file" form of
  Just FileData { fdPayload, fdFileName, fdInputName } -> do
    Storage.IPFS.addFile fdPayload (name queryName fdInputName fdFileName "foo.txt") >>= \case
      Left _       -> throwM $ err500 { errBody = "IPFS add error" }
      Right struct -> cont struct

  Nothing ->
    throwM $ err422 { errBody = "File not processable" }
  where
    name :: Maybe IPFS.Name -> Text -> Text -> String -> IPFS.Name
    name queryName' inputName fileName fallback =
      case queryName' of
        Nothing              -> IPFS.Name $ name' inputName fileName fallback
        Just (IPFS.Name "")  -> IPFS.Name $ name' inputName fileName fallback
        Just ipfsName        -> ipfsName

    name' :: Text -> Text -> String -> String
    name' ""        ""       fallback = fallback
    name' ""        fileName _        = Text.unpack fileName
    name' inputName _        _        = Text.unpack inputName
