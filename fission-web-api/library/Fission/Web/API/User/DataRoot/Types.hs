module Fission.Web.API.User.DataRoot.Types (Routes (..)) where

import qualified Network.IPFS.CID.Types      as IPFS

import           Fission.User.Username.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types  as Auth

data Routes mode = Routes
  { get ::
      mode
      :- Summary "Get Data Root"
      :> Description "Retrieve a user's data root from DNS"
      --
      :> Capture "username" Username
      :> Get '[JSON, PlainText] IPFS.CID

  , update ::
      mode
      :- Summary "Update data root"
      :> Description "Set/update currently authenticated user's file system content"
      --
      :> Capture "newCID" IPFS.CID
      --
      :> Auth.HigherOrder
      :> PatchNoContent
  }
  deriving Generic
