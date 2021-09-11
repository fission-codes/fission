module Fission.Web.API.User.DataRoot.Types (RoutesV2 (..), RoutesV_ (..)) where

import qualified Network.IPFS.CID.Types      as IPFS

import           Fission.User.Username.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types  as Auth

data RoutesV2 mode = RoutesV2
  { get    :: mode :- GetCID
  , update :: mode :- Update PutNoContent
  }
  deriving Generic

data RoutesV_ mode = RoutesV_
  { get    :: mode :- GetCID
  , update :: mode :- Update PatchNoContent
  }
  deriving Generic

type GetCID
  =  Summary "Get Data Root"
  :> Description "Retrieve a user's data root from DNS"
  --
  :> Capture "username" Username
  :> Get '[JSON, PlainText] IPFS.CID

type Update response
  =  Summary "Update data root"
  :> Description "Set/update currently authenticated user's file system content"
  --
  :> Capture "newCID" IPFS.CID
  --
  :> Auth.HigherOrder
  :> response
