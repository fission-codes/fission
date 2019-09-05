module Fission.Web.IPFS.Pin
  ( API
  , server
  , pin
  , unpin
  ) where

import RIO
import RIO.Process (HasProcessContext)

import           Data.Has
import           Database.Selda
import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.Internal.Orphanage ()
import qualified Fission.Storage.IPFS as Storage.IPFS
import           Fission.User.CID     as UserCID
import           Fission.User
import qualified Fission.Web.Error    as Web.Err
import           Fission.Web.Server

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Client    as IPFS.Client
import qualified Fission.IPFS.Types     as IPFS

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent

server :: Has IPFS.URL      cfg
       => Has HTTP.Manager  cfg
       => HasProcessContext cfg
       => MonadSelda   (RIO cfg)
       => HasLogFunc        cfg
       => User
       -> RIOServer         cfg API
server User { _userID } = pin _userID :<|> unpin _userID

pin :: Has IPFS.URL      cfg
    => Has HTTP.Manager  cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => ID User
    -> RIOServer         cfg PinAPI
pin uID _cid@(CID hash) = IPFS.Client.run (IPFS.Client.pin hash) >>= \case
  Left err -> Web.Err.throw err
  Right _  -> UserCID.create uID _cid >> pure NoContent

unpin :: Has IPFS.URL      cfg
      => Has HTTP.Manager  cfg
      => HasLogFunc        cfg
      => MonadSelda   (RIO cfg)
      => ID User
      -> RIOServer         cfg UnpinAPI
unpin uID _cid@CID { unaddress = hash } = do
  void $ deleteFrom_ userCIDs (eqUserCID uID hash)

  remaining <- query
            . limit 0 1
            $ select userCIDs `suchThat` eqUserCID uID hash

  when (null remaining) do
    resp <- IPFS.Client.run $ IPFS.Client.unpin hash False
    void $ Web.Err.ensure resp

  return NoContent
