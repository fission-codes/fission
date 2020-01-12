module Fission.Web.IPFS.DAG
  ( API
  , put
  )
where

import           Database.Esqueleto
import           Servant

import           Fission.Models
import           Fission.Prelude

import           Fission.User.CID.Mutation as User.CID
import qualified Fission.Web.Error  as Web.Err

import           Network.IPFS
import           Network.IPFS.File.Types as File
import qualified Network.IPFS.Types      as IPFS
import qualified Network.IPFS.DAG        as IPFS.DAG
import qualified Network.IPFS.Pin        as IPFS.Pin

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

put ::
  ( -- User.CID.MonadDBMutation m
   MonadLocalIPFS           m
  , MonadRemoteIPFS          m
  , MonadLogger              m
  , MonadThrow               m
  , MonadTime                m
  , MonadDB                  m
  )
  => Entity User
  -> ServerT API m
put (Entity userId _) (Serialized rawData) =
  IPFS.DAG.put rawData >>= \case
    Left err ->
      Web.Err.throw err

    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Left err ->
          Web.Err.throw err

        Right pinnedCID -> do
          _ <- runDBNow (\now -> User.CID.createX userId [newCID] now)
          return pinnedCID
