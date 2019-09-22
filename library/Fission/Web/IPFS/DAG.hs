module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           RIO
import           RIO.Process (HasProcessContext)

import           SuperRecord
import           Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.User
import           Fission.User.CID.Mutation as User.CID
import           Fission.Web.Server
import           Fission.File.Types   as File

import qualified Fission.IPFS.Types      as IPFS
import qualified Fission.Storage.IPFS.DAG    as Storage.IPFS.DAG
import qualified Fission.Web.Error       as Web.Err

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

put :: Has "ipfsPath" cfg IPFS.BinPath
    => Has "ipfsTimeout" cfg IPFS.Timeout
    => Has "httpManager" cfg HTTP.Manager
    => Has "ipfsURL" cfg IPFS.URL
    => HasProcessContext (Rec cfg)
    => MonadSelda   (RIO (Rec cfg))
    => HasLogFunc        (Rec cfg)
    => User
    -> RIOServer         (Rec cfg) API
put User { _userID } (Serialized rawData) = Storage.IPFS.DAG.put rawData >>= \case
  Right newCID -> do
    void $ User.CID.createX _userID [newCID]
    return newCID

  Left err ->
    Web.Err.throw err
