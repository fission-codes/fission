module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Database.Selda
import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.Prelude
import           Fission.User
import           Fission.User.CID.Mutation as User.CID
import           Fission.Web.Server
import           Fission.File.Types   as File

import qualified Fission.IPFS.Types      as IPFS
import qualified Fission.Storage.IPFS.DAG    as IPFS.DAG
import qualified Fission.Storage.IPFS.Pin    as IPFS.Pin
import qualified Fission.Web.Error       as Web.Err

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

put
  :: ( Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     , HasProcessContext cfg
     , MonadSelda   (RIO cfg)
     , HasLogFunc        cfg
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
