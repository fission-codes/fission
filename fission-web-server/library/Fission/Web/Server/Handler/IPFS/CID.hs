module Fission.Web.Server.Handler.IPFS.CID (handler) where

import           Database.Esqueleto
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.CID.Types         as CID

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.LoosePin            as LoosePin
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB

handler :: (MonadDB t m, LoosePin.Retriever t) => CID.Routes (AsServerT m)
handler = CID.Routes {..}
  where
    allForUser Authorization {about = Entity userId _} =
      runDB do
        pins <- LoosePin.getByUserId userId
        return (getInner loosePinCid <$> pins)
