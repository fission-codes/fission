module Fission.Web.IPFS.Pin
  ( API
  , PinAPI
  , UnpinAPI
  , server
  , pin
  , unpin
  ) where

import Flow
import RIO

import Data.Has
import Database.Selda

import qualified Network.HTTP.Client      as HTTP
import           Servant

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

server :: Has HTTP.Manager  cfg
       => Has IPFS.URL      cfg
       => MonadSelda   (RIO cfg)
       => HasLogFunc        cfg
       => User
       -> RIOServer         cfg API
server User { userID } = pin userID :<|> unpin userID

pin :: Has HTTP.Manager  cfg
    => Has IPFS.URL      cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => ID User
    -> RIOServer         cfg PinAPI
pin uID _cid = IPFS.Pin.add _cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> do
    UserCID.create uID _cid
    pure NoContent

unpin :: Has HTTP.Manager  cfg
      => Has IPFS.URL      cfg
      => HasLogFunc        cfg
      => MonadSelda   (RIO cfg)
      => ID User
      -> RIOServer         cfg UnpinAPI
unpin uID _cid@CID { unaddress = hash } = do
  hash
    |> eqUserCID uID
    |> deleteFrom_ userCIDs
    |> void

  remaining <- query
                 <| limit 0 1
                 <| select userCIDs `suchThat` eqUserCID uID hash

  when (null remaining) $ IPFS.Pin.rm _cid >>= void . Web.Err.ensure

  return NoContent
