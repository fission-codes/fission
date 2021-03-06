module Fission.Web.Server.Handler.IPFS.Upload (add) where

import           Database.Esqueleto

import qualified Network.IPFS.Client.Pin                as IPFS.Pin
import           Network.IPFS.File.Types                as File
import           Network.IPFS.Remote.Class              as IPFS

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Upload.Types      as API.IPFS

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.LoosePin.Creator    as LoosePin
import           Fission.Web.Server.MonadDB

add ::
  ( MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => ServerT API.IPFS.Upload m
add (Serialized rawData) Authorization {about = Entity userId _} =
  IPFS.ipfsAdd rawData >>= \case
    Right newCID ->
      IPFS.ipfsPin newCID >>= \case
        Right _ -> do
          runDBNow $ LoosePin.createMany userId [newCID]
          return newCID

        Left err ->
          Web.Err.throw err

    Left err ->
      Web.Err.throw err
