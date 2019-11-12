module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Flow
import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has
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

put :: Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => Has HTTP.Manager  cfg
    => Has IPFS.URL      cfg
    => HasProcessContext cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => User
    -> RIOServer         cfg API
put User { userID } (Serialized rawData) = Storage.IPFS.DAG.put rawData >>= \case
  Right newCID -> do
    [newCID]
      |> User.CID.createX userID
      |> void

    return newCID

  Left err ->
    Web.Err.throw err
