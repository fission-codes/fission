module Network.IPFS.Client.Stat (API) where

import           Servant.API

import qualified Network.IPFS.Client.Param as Param
import           Network.IPFS.Stat.Types

-- IPFS v0.5 disallows GET requests
-- https://docs.ipfs.io/recent-releases/go-ipfs-0-5/#breaking-changes-upgrade-notes
type API = "stat"
        :> Param.CID'
        :> Post '[JSON] Stat
