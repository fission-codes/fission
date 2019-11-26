module Fission.Web.IPFS.Pin
  ( API
  , PinAPI
  , UnpinAPI
  , server
  , pin
  , unpin
  ) where

import Fission.Prelude

import Database.Selda

import           Servant

import           Network.IPFS
import qualified Network.IPFS.Types as IPFS
import qualified Network.IPFS.Pin   as IPFS.Pin
import           Network.IPFS.CID.Types

import qualified Fission.Web.Error        as Web.Err
import           Fission.Web.Server
import           Fission.User.CID         as UserCID
import           Fission.User

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent

server ::
  ( MonadRemoteIPFS (RIO cfg)
  , MonadSelda      (RIO cfg)
  , HasLogFunc           cfg
  )
  => User
  -> RIOServer cfg API
server User { userID } = pin userID :<|> unpin userID

pin ::
  ( MonadRemoteIPFS (RIO cfg)
  , MonadSelda      (RIO cfg)
  , HasLogFunc           cfg
  )
  => ID User
  -> RIOServer cfg PinAPI
pin uID _cid = IPFS.Pin.add _cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> do
    UserCID.create uID _cid
    pure NoContent

unpin ::
  ( MonadRemoteIPFS (RIO cfg)
  , MonadSelda      (RIO cfg)
  , HasLogFunc           cfg
  )
  => ID User
  -> RIOServer cfg UnpinAPI
unpin uID cid@CID { unaddress = hash } = do
  hash
    |> eqUserCID uID
    |> deleteFrom_ userCIDs
    |> void

  remaining <- query
                 <| limit 0 1
                 <| select userCIDs `suchThat` eqUserCID uID hash

  when (null remaining) do
    result <- IPFS.Pin.rm cid

    result
      |> Web.Err.ensure
      |> void

  return NoContent
