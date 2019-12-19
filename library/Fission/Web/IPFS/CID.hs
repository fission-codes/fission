module Fission.Web.IPFS.CID
  ( API
  , allForUser
  ) where

import Database.Esqueleto
import Network.IPFS.CID.Types as IPFS.CID
import Servant

import Fission.Prelude
import Fission.Models

type API = Get '[JSON, PlainText] [CID]

allForUser :: MonadDB m => Entity User -> ServerT API m
allForUser (Entity userId _) = runDB do
  hashes <- select <| from \userCid -> do
    where_ (userCid ^. UserCidUserFk ==. val userId)
    return (userCid ^. UserCidCid)

  return (unValue <$> hashes)
