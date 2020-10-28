module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types                     as IPFS.CID
import           Servant

import           Fission.Prelude

import qualified Fission.Authorization                      as Authorization
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
  => Authorization.Session
  -> ServerT API m
allForUser Authorization.Session {} = runDB do
-- allForUser Authorization {about = Entity userId _} = runDB do
  let userId = undefined
  pins <- LoosePin.getByUserId userId
  return (getInner loosePinCid <$> pins)
