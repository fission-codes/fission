module Fission.Web.IPFS.Upload.Multipart
  ( API
  , FileRequest
  , NameQuery
  , add
  , jsonAdd
  , textAdd
  ) where

import           RIO
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import           SuperRecord
import           Database.Selda

import qualified Network.HTTP.Client as HTTP
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

add :: Has "ipfsPath" cfg IPFS.BinPath
    => Has "ipfsTimeout" cfg IPFS.Timeout
    => Has "httpManager" cfg HTTP.Manager
    => Has "ipfsURL" cfg IPFS.URL
    => MonadSelda   (RIO (Rec cfg))
    => HasProcessContext (Rec cfg)
    => HasLogFunc        (Rec cfg)
    => User
    -> RIOServer         (Rec cfg) API
add User { _userID } = textAdd _userID :<|> jsonAdd _userID

textAdd :: HasOf [ "ipfsPath"    := IPFS.BinPath
                , "ipfsTimeout" := IPFS.Timeout
                , "httpManager" := HTTP.Manager
                , "ipfsURL"     := IPFS.URL
                ] cfg
        => HasProcessContext (Rec cfg)
        => MonadSelda   (RIO (Rec cfg))
        => HasLogFunc        (Rec cfg)
        => ID User
        -> RIOServer         (Rec cfg) TextAPI
textAdd uID form queryName = run uID form queryName $ \sparse ->
  case IPFS.linearize sparse of
    Right hash -> pure hash
    Left err   -> Web.Err.throw err

jsonAdd :: MonadSelda   (RIO (Rec cfg))
        => HasOf [ "ipfsPath"    := IPFS.BinPath
                , "ipfsTimeout" := IPFS.Timeout
                , "httpManager" := HTTP.Manager
                , "ipfsURL"     := IPFS.URL
                ] cfg
        => HasProcessContext (Rec cfg)
        => HasLogFunc        (Rec cfg)
        => ID User
        -> RIOServer         (Rec cfg) JSONAPI
jsonAdd uID form queryName = run uID form queryName pure

run :: MonadRIO          (Rec cfg) m
    => MonadMask             m
    => MonadSelda            m
    => HasOf [ "httpManager" := HTTP.Manager
            , "ipfsTimeout" := IPFS.Timeout
            , "httpManager" := HTTP.Manager
            , "ipfsURL"     := IPFS.URL
            , "unusedConstraint" := Int
            ] cfg
    => HasProcessContext (Rec cfg)
    => HasLogFunc        (Rec cfg)
    => ID User
    -> MultipartData Mem
    -> Maybe IPFS.Name
    -> (IPFS.SparseTree -> m a)
    -> m a
run uID form qName cont = case lookupFile "file" form of
  Nothing -> throwM $ err422 { errBody = "File not processable by IPFS" }
  Just FileData { .. } ->
    Storage.IPFS.addFile fdPayload humanName >>= \case
      Left err ->
        Web.Err.throw err

      Right struct -> do
        void $ User.CID.createX uID (IPFS.cIDs struct)
        cont struct
    where
      humanName :: IPFS.Name
      humanName = toName qName fdFileName fdFileCType

toName :: Maybe IPFS.Name -> Text -> Text -> IPFS.Name
toName queryName' fileName mime =
  case queryName' of
    Nothing              -> IPFS.Name $ plainName fileName mime
    Just (IPFS.Name "")  -> IPFS.Name $ plainName fileName mime
    Just ipfsName        -> ipfsName

plainName :: Text -> Text -> String
plainName ""       mime = Text.unpack $ "file." <> lookupExt (encodeUtf8 mime)
plainName fileName _    = Text.unpack fileName
