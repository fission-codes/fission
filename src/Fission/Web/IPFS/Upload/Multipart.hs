module Fission.Web.IPFS.Upload.Multipart
  ( API
  , add
  , jsonAdd
  , textAdd
  ) where

import           RIO
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import           Data.Has
import           Database.Selda

import           Servant
import           Servant.Multipart

import           Fission.Internal.Constraint
import           Fission.Internal.MIME

import           Fission.User
import           Fission.User.CID.Mutation as User.CID
import           Fission.Web.Server

import qualified Fission.IPFS.SparseTree as IPFS
import qualified Fission.IPFS.Types      as IPFS
import qualified Fission.Storage.IPFS    as Storage.IPFS
import qualified Fission.Web.Error       as Web.Err

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
    => MonadSelda   (RIO cfg)
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => User
    -> RIOServer         cfg API
add User { _userID } = textAdd _userID :<|> jsonAdd _userID

textAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => MonadSelda   (RIO cfg)
        => HasLogFunc        cfg
        => ID User
        -> RIOServer         cfg TextAPI
textAdd uID form queryName = run uID form queryName $ \sparse ->
  case IPFS.linearize sparse of
    Right hash -> pure hash
    Left err   -> Web.Err.throw err

jsonAdd :: MonadSelda   (RIO cfg)
        => Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => ID User
        -> RIOServer         cfg JSONAPI
jsonAdd uID form queryName = run uID form queryName pure

run :: MonadRIO          cfg m
    => MonadThrow            m
    => MonadSelda            m
    => Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => ID User
    -> MultipartData Mem
    -> Maybe IPFS.Name
    -> (IPFS.SparseTree -> m a)
    -> m a
run uID form qName cont = case lookupFile "file" form of
  Nothing -> throwM $ err422 { errBody = "File not processable by IPFS" }
  Just FileData { .. } ->
    Storage.IPFS.addFile fdPayload humanName >>= \case
      Left err     -> Web.Err.throw err
      Right struct -> User.CID.insertNewX uID (IPFS.cids struct) >> cont struct
    where
      humanName :: IPFS.Name
      humanName = name qName fdFileName fdFileCType

name :: Maybe IPFS.Name -> Text -> Text -> IPFS.Name
name queryName' fileName mime =
  case queryName' of
    Nothing              -> IPFS.Name $ plainName fileName mime
    Just (IPFS.Name "")  -> IPFS.Name $ plainName fileName mime
    Just ipfsName        -> ipfsName

plainName :: Text -> Text -> String
plainName ""       mime = Text.unpack $ "file." <> lookupExt (encodeUtf8 mime)
plainName fileName _    = Text.unpack fileName
