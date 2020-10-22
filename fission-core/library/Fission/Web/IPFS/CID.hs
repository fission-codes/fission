module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types                     as IPFS.CID
import           Servant

import           Fission.Prelude

import           Fission.Authorization.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.LoosePin                           as LoosePin
import           Fission.Models

type API
  =  Summary "CID Index"
  :> Description "List of all of your pinned CIDs (not associated with your personal file system or apps)"
  :> Get '[JSON, PlainText] [CID]

allForUser ::
  ( MonadDB            t m
  , LoosePin.Retriever t
  )
  => Authorization [Resource]
  -> ServerT API m
allForUser Authorization {about = Entity userId _} = runDB do
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
