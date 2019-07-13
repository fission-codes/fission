module Fission.Web.IPFS.Pin
  ( API
  , server
  , pin
  , unpin
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Bifunctor
import Data.Has
import Data.Time
import Database.Selda
import Servant

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import qualified Fission.Web.Error    as Web.Err
import           Fission.Web.Server
import           Fission.IPFS.CID.Types
import           Fission.User.CID     as UserCID
import           Fission.User

import Fission.User.CID
import Fission.Timestamp

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> Delete '[PlainText, OctetStream] NoContent

server :: Has IPFS.BinPath  cfg
       => HasProcessContext cfg
       => MonadSelda   (RIO cfg)
       => HasLogFunc        cfg
       => User
       -> RIOServer         cfg API
server User { _userID } = pin _userID :<|> unpin _userID

pin :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => ID User
    -> RIOServer         cfg PinAPI
pin uID cid@(CID hash) = Storage.IPFS.pin cid >>= \case
  Left err -> Web.Err.throw err
  Right ()  -> do
    now <- liftIO getCurrentTime

    transaction $
      insertUnless userCIDs (eqUserCID uID hash)
        [UserCID def uID hash <@ now]

    return NoContent

unpin :: Has IPFS.BinPath  cfg
      => HasProcessContext cfg
      => HasLogFunc        cfg
      => MonadSelda   (RIO cfg)
      => ID User
      -> RIOServer         cfg UnpinAPI
unpin uID cid@(CID { unaddress = hash }) =
  transaction do
    deleteFrom_ userCIDs (eqUserCID uID hash)
    when lookupAnyMatchingCID $ Storage.IPFS.unpin cid

  return NoContent
