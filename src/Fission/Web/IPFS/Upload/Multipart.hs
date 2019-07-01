module Fission.Web.IPFS.Upload.Multipart
  ( API
  , add
  , jsonAdd
  , textAdd
  ) where

import RIO
import RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Data.Has
import Servant
import Servant.Multipart

import           Fission.Web.Server
import           Fission.Internal.Constraint
import           Fission.Internal.MIME
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.IPFS.SparseTree       as IPFS
import qualified Fission.Storage.IPFS          as Storage.IPFS
-- import qualified Fission.IPFS.Error            as IPFS.Error
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

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
add = textAdd :<|> jsonAdd

textAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg TextAPI
textAdd form queryName = run form queryName $ \sparse ->
  case IPFS.linearize sparse of
    Right x   -> pure x
    Left  err -> IPFS.throwLinear err

jsonAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg JSONAPI
jsonAdd form queryName = run form queryName pure

run :: MonadRIO          cfg m
    => MonadThrow            m
    => Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => MultipartData Mem
    -> Maybe IPFS.Name
    -> (IPFS.SparseTree -> m a)
    -> m a
run form qName cont = case lookupFile "file" form of
  Nothing -> throwM $ err422 { errBody = "File not processable by IPFS" }
  Just FileData { .. } -> do
    Storage.IPFS.addFile fdPayload humanName >>= \case
      Right struct -> cont struct
      Left  err    -> do
        logError $ displayShow err
        throwM $ err500 { errBody = "IPFS add error" }
    where
      humanName :: IPFS.Name
      humanName = name qName fdFileName fdFileCType

name :: Maybe IPFS.Name -> Text -> Text -> IPFS.Name
name queryName' fileName mime =
  case queryName' of
    Nothing              -> IPFS.Name $ name' fileName mime
    Just (IPFS.Name "")  -> IPFS.Name $ name' fileName mime
    Just ipfsName        -> ipfsName

name' :: Text -> Text -> String
name' ""       mime = Text.unpack $ "file." <> lookupExt (encodeUtf8 mime)
name' fileName _    = Text.unpack fileName
