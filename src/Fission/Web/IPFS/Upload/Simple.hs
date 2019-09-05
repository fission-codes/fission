module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import           Data.Has
import           Database.Selda
import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import           Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import           Fission.User
import           Fission.User.CID.Mutation as UserCID

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Client    as IPFS.Client
import qualified Fission.IPFS.Types     as IPFS

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add :: Has IPFS.URL      cfg
    => Has HTTP.Manager  cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => User
    -> RIOServer         cfg API
add User { _userID } (Serialized rawData) = IPFS.Client.run (IPFS.Client.add rawData) >>= \case
  Right newCID -> do
    void . transaction $ UserCID.createX _userID [newCID]
    return newCID

  Left err ->
    Web.Err.throw err
