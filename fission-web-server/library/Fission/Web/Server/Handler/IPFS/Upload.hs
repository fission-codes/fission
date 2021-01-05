module Fission.Web.Server.Handler.IPFS.Upload (add) where

import           Database.Esqueleto

import           Network.IPFS
import qualified Network.IPFS.Add                       as IPFS
import           Network.IPFS.File.Types                as File
import qualified Network.IPFS.Pin                       as IPFS.Pin
import qualified Network.IPFS.Types                     as IPFS

import           Servant

import           Fission.Prelude

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.LoosePin.Creator    as LoosePin

add ::
  ( MonadLocalIPFS     m
  , MonadRemoteIPFS    m
  , MonadLogger        m
  , MonadThrow         m
  , MonadTime          m
  , MonadDB          t m
  , LoosePin.Creator t
  )
  => Authorization
  -> ServerT API m
add Authorization {about = Entity userId _} (Serialized rawData) =
  IPFS.addRaw rawData >>= \case
    Right newCID ->
      IPFS.Pin.add newCID >>= \case
        Right pinnedCID -> do
          runDBNow $ LoosePin.createMany userId [pinnedCID]
          return pinnedCID

        Left err ->
          Web.Err.throw err

    Left err ->
      Web.Err.throw err
