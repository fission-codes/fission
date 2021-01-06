module Fission.Web.Server.Handler.IPFS.CID (allForUser) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.CID.Types         as API.IPFS

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.LoosePin            as LoosePin
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB

allForUser :: (MonadDB t m, LoosePin.Retriever t) => ServerT API.IPFS.CID m
allForUser Authorization {about = Entity userId _} = runDB do
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
