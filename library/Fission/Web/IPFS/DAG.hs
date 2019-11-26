module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Database.Selda
import           Servant

import           Fission.Prelude
import           Fission.User
import           Fission.User.CID.Mutation as User.CID
import           Fission.Web.Server
import qualified Fission.Web.Error  as Web.Err

import           Network.IPFS

import           Network.IPFS.File.Types as File
import qualified Network.IPFS.Types      as IPFS
import qualified Network.IPFS.DAG        as IPFS.DAG
import qualified Network.IPFS.Pin        as IPFS.Pin


type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

put ::
  ( MonadSelda      (RIO cfg)
  , MonadLocalIPFS  (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , HasLogFunc           cfg
  )
  => User
  -> RIOServer cfg API
put User { userID } (Serialized rawData) = IPFS.DAG.put rawData >>= \case
  Right newCID -> do
    _ <- User.CID.createX userID [newCID]
    
    IPFS.Pin.add newCID >>= \case
      Right newCID' -> return newCID'
      Left err -> Web.Err.throw err

  Left err ->
    Web.Err.throw err
