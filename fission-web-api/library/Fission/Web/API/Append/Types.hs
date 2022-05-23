-- module Fission.Web.API.Append.Types (RoutesV_ (..), RoutesV2 (..)) where
module Fission.Web.API.Append.Types (RoutesV2 (..)) where

import           Network.IPFS.File.Types                  as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types              as Auth

data RoutesV2 mode = RoutesV2
  { append ::
      mode
      :- Summary     "Append a file"
      :> Description "Append a file to an app's public upload directory"
      --
      :> Capture    "Username"  String
      :> Capture    "Creator"   String
      :> Capture    "App Name"  String
      :> Capture    "File Name" String
      --
      :> ReqBody '[OctetStream] Serialized
      --
      :> Auth.HigherOrder
      :> PutNoContent
  }
  deriving Generic