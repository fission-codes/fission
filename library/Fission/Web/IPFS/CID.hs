module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import Database.Esqueleto
import Network.IPFS.CID.Types as IPFS.CID
import Servant

import Fission.Prelude
import Fission.Models
import Fission.User.CID.Query as UserCIDQuery

type API = Get '[JSON, PlainText] [CID]

allForUser :: MonadDB m => Entity User -> ServerT API m
allForUser (Entity userId _) = runDB do
  userCids <- UserCIDQuery.getByUserId userId
  let cids = getInner userCidCid <$> userCids
  return cids
