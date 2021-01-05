module Fission.Web.Server.Handler.IPFS.CID (allForUser) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types                 as IPFS.CID
import           Servant

import           Fission.Prelude

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.LoosePin            as LoosePin
import           Fission.Web.Server.Models

allForUser :: (MonadDB t m, LoosePin.Retriever t) => Authorization -> ServerT API m
allForUser Authorization {about = Entity userId _} = runDB do
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
