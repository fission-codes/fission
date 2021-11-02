module Network.IPFS.Client.DAG.Types (API) where

import           Servant.API

import qualified Network.IPFS.Client.DAG.Put.Types as Put

type API
  =  "put"
  :> Put.API
