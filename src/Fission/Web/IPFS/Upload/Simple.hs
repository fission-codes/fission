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
import           Fission.User.CID     as User.CID

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => ID User
    -> RIOServer         cfg API
add uid (Serialized rawData) = Storage.IPFS.addRaw rawData >>= \case
  Left err ->
    Web.Err.throw err

  Right newCID@(IPFS.CID hash) ->
    transaction do
      results <- query $ select userCIDs >>= inUserCIDs uID hashes
      when (results == []) (void $ User.CID.createFresh uid newCID)
      return newCID
