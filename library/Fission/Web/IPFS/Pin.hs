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

import qualified Network.HTTP.Client      as HTTP
import           Servant

import qualified Fission.IPFS.Types       as IPFS
import qualified Fission.Storage.IPFS.Pin as IPFS.Pin
import qualified Fission.Web.Error        as Web.Err
import           Fission.Web.Server
import           Fission.IPFS.CID.Types
import           Fission.User.CID         as CID
import           Fission.User


-- API


type API = PinAPI :<|> UnpinAPI



-- SERVER


server
  :: ( Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     , MonadSelda   (RIO cfg)
     , HasLogFunc        cfg
     )
  => User
  -> RIOServer cfg API
server User { userID } = pin userID :<|> unpin userID



-- PIN


type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent


pin
  :: ( Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     , MonadSelda   (RIO cfg)
     , HasLogFunc        cfg
     )
  => UserId
  -> RIOServer cfg PinAPI
pin userId cid = IPFS.Pin.add cid >>= \case
  Left err ->
    Web.Err.throw err

  Right _  -> do
    UserCID.create userId cid
    pure NoContent



-- UNPIN


type UnpinAPI = Capture "cid" CID
             :> DeleteAccepted '[PlainText, OctetStream] NoContent


unpin
  :: ( Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     , HasLogFunc        cfg
     , MonadSelda   (RIO cfg)
     )
  => UserId
  -> RIOServer cfg UnpinAPI
unpin userId cid@CID { unaddress = hash } = do
  -- Remove all the matching CIDs from the database
  Query.deleteWhere (\asset ->
    asset ^. userFK ==. userId ++
    asset ^. cid ==. hash
  )

  -- Check if any other user has pinned this CID
  assetFromOtherUser <- Query.oneWhere (\asset ->
    asset ^. cid ==. hash
  )

  -- Unpin from IPFS if nobody has this CID asset
  case assetFromOtherUser of
    Just _ ->
      void

    Nothing ->
      Web.Err.ensure (IPFS.Pin.rm cid)

  -- Empty response
  return NoContent
