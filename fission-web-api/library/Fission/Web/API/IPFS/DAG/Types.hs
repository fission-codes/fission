module Fission.Web.API.IPFS.DAG.Types (DAG) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.IPFS.DAG.Upload.Types

type DAG = "dag" :> Upload
