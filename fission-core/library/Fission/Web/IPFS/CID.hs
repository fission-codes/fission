module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types as IPFS.CID
import           Servant

import           Fission.Prelude

import           Fission.Authorization.Types

import           Fission.Models
import qualified Fission.LoosePin as LoosePin

type API
  =  Summary "CID Index"
  :> Description "List of all of your pinned CIDs (not associated with your personal file system or apps)"
  :> Get '[JSON, PlainText] [CID]

allForUser :: (MonadDB t m, LoosePin.Retriever t) => Authorization -> ServerT API m
allForUser Authorization {about = Entity userId _} = runDB do
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
