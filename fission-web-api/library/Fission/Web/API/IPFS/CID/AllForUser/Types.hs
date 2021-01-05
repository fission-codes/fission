module Fission.Web.API.IPFS.CID.AllForUser.Types (AllForUser) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type AllForUser
  =  Summary "CID Index"
  :> Description "List of all of your pinned CIDs (not associated with your personal file system or apps)"
  --
  :> Auth.HigherOrder
  :> Get '[JSON, PlainText] [IPFS.CID]
