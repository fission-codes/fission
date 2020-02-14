module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types as IPFS.CID
import           Servant

import           Fission.Prelude
import           Fission.Models
import qualified Fission.LoosePin as LoosePin

type API = Get '[JSON, PlainText] [CID]

allForUser ::
  ( MonadDB            t m
  , LoosePin.Retriever t
  )
  => Entity User -> ServerT API m
allForUser (Entity userId _) = runDB do
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
