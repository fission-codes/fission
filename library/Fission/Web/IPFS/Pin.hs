module Fission.Web.IPFS.Pin
  ( API
  , server
  , pin
  , unpin
  ) where

import RIO

import Data.Has
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import qualified Fission.Web.Error    as Web.Err
import           Fission.Web.Server
import           Fission.IPFS.CID.Types
import           Fission.User.CID     as UserCID
import           Fission.User

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent

server :: Has HTTP.Manager  cfg
       => Has IPFS.URL      cfg
       => MonadSelda   (RIO cfg)
       => HasLogFunc        cfg
       => User
       -> RIOServer         cfg API
server User { _userID } = pin _userID :<|> unpin _userID

pin :: Has HTTP.Manager  cfg
    => Has IPFS.URL      cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => ID User
    -> RIOServer         cfg PinAPI
pin uID _cid = Storage.IPFS.pin _cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> UserCID.create uID _cid >> pure NoContent

unpin :: Has HTTP.Manager  cfg
      => Has IPFS.URL      cfg
      => HasLogFunc        cfg
      => MonadSelda   (RIO cfg)
      => ID User
      -> RIOServer         cfg UnpinAPI
unpin uID _cid@CID { unaddress = hash } = do
  void $ deleteFrom_ userCIDs (eqUserCID uID hash)

  remaining <- query
            . limit 0 1
            $ select userCIDs `suchThat` eqUserCID uID hash

  when (null remaining) $ Storage.IPFS.unpin _cid >>= void . Web.Err.ensure

  return NoContent
