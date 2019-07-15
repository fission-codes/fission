module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda
import Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import           Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import           Fission.User
import           Fission.User.CID.Mutation as UserCID

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => User
    -> RIOServer         cfg API
add User { _userID } (Serialized rawData) = Storage.IPFS.addRaw rawData >>= \case
  Right newCID -> do
    void . transaction $ UserCID.createX _userID [newCID]
    return newCID

  Left err ->
    Web.Err.throw err
