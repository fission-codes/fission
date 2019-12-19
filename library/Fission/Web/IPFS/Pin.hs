module Fission.Web.IPFS.Pin
  ( API
  , PinAPI
  , UnpinAPI
  , server
  , pin
  , unpin
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import qualified Network.IPFS.Pin   as IPFS.Pin
import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.Web.Error         as Web.Err
import           Fission.User.CID.Mutation as UserCID
import           Fission.Models

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent

server ::
  ( MonadRemoteIPFS m
  , MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  , MonadDB         m
  , MonadReader cfg m
  )
  => Entity User
  -> ServerT API m
server (Entity userId _) = pin userId :<|> unpin userId

pin ::
  ( MonadRemoteIPFS m
  , MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  , MonadDB         m
  , MonadReader cfg m
  )
  => UserId
  -> ServerT PinAPI m
pin userId cid = IPFS.Pin.add cid >>= \case
  Left err -> Web.Err.throw err
  Right _  -> do
    UserCID.create userId cid
    pure NoContent

unpin ::
  ( MonadRemoteIPFS m
  , MonadLogger     m
  , MonadThrow      m
  , MonadDB         m
  , MonadReader cfg m
  )
  => UserId
  -> ServerT UnpinAPI m
unpin userId cid = do
  remaining <- runDB do
    delete <| from \userCID ->
      where_ (selectExact userCID)

    select <| from \userCID -> do
      where_ (selectExact userCID)
      limit 1
      return userCID

  when (null remaining) do
    void <| Web.Err.ensure =<< IPFS.Pin.rm cid

  return NoContent
  where
    selectExact userCID =
          userCID ^. UserCIDCid    ==. val cid
      &&. userCID ^. UserCIDUserFk ==. val userId
