module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import Database.Esqueleto
import Network.IPFS.CID.Types as IPFS.CID
import Servant

import Fission.Prelude
import Fission.Models
import qualified Fission.User.CID as User.CID

type API = Get '[JSON, PlainText] [CID]

allForUser :: User.CID.MonadDBQuery m => Entity User -> ServerT API m
allForUser (Entity userId _) = runDB do
  userCids <- User.CID.getByUserId userId
  return (getInner userCidCid <$> userCids)

