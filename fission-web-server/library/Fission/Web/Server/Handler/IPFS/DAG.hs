module Fission.Web.Server.Handler.IPFS.DAG (put) where

import           Database.Esqueleto

import           Network.IPFS
import qualified Network.IPFS.DAG                       as IPFS.DAG
import           Network.IPFS.File.Types                as File
import qualified Network.IPFS.Pin                       as IPFS.Pin

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.DAG.Upload.Types  as API.DAG

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.LoosePin.Creator    as LoosePin
import           Fission.Web.Server.MonadDB

put ::
  ( MonadRemoteIPFS    m
  , MonadLocalIPFS     m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => ServerT API.DAG.Upload m
put (Serialized rawData) Authorization {about = Entity userId _} =
  IPFS.DAG.put rawData >>= \case
    Left err ->
      Web.Err.throw err

    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Left err ->
          Web.Err.throw err

        Right pinnedCID -> do
          newCID
            |> return
            |> LoosePin.createMany userId
            |> runDBNow

          return pinnedCID
