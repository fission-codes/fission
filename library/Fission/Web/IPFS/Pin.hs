module Fission.Web.IPFS.Pin
  ( API
  , PinAPI
  , UnpinAPI
  , server
  , pin
  , unpin
  ) where

import RIO

import SuperRecord
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant
import qualified Servant.Client      as Client

import qualified Fission.IPFS.Types       as IPFS
import qualified Fission.Storage.IPFS.Pin as IPFS.Pin
import qualified Fission.Web.Error        as Web.Err
import           Fission.Web.Server
import           Fission.IPFS.CID.Types
import           Fission.User.CID         as UserCID
import           Fission.User

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent

server :: Has "httpManager" cfg HTTP.Manager
       => Has "ipfsURL"     cfg Client.BaseUrl
       => MonadSelda   (RIO (Rec cfg))
       => HasLogFunc        (Rec cfg)
       => User
       -> RIOServer         (Rec cfg) API
server User { _userID } = pin _userID :<|> unpin _userID

pin :: Has "httpManager" cfg HTTP.Manager
    => Has "ipfsURL"     cfg Client.BaseUrl
    => MonadSelda   (RIO (Rec cfg))
    => HasLogFunc        (Rec cfg)
    => ID User
    -> RIOServer         (Rec cfg) PinAPI
pin uID _cid = IPFS.Pin.add _cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> UserCID.create uID _cid >> pure NoContent

unpin :: Has "httpManager" cfg HTTP.Manager
      => Has "ipfsURL"     cfg Client.BaseUrl
      => HasLogFunc        (Rec cfg)
      => MonadSelda   (RIO (Rec cfg))
      => ID User
      -> RIOServer         (Rec cfg) UnpinAPI
unpin uID _cid@CID { unaddress = hash } = do
  void $ deleteFrom_ userCIDs (eqUserCID uID hash)

  remaining <- query
            . limit 0 1
            $ select userCIDs `suchThat` eqUserCID uID hash

  when (null remaining) $ IPFS.Pin.rm _cid >>= void . Web.Err.ensure

  return NoContent
