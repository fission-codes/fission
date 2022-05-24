-- module Fission.Web.API.Append.Types (RoutesV_ (..), RoutesV2 (..)) where
module Fission.Web.API.Append.Types (RoutesV2 (..)) where

import           Network.IPFS.File.Types                 as File

import           RIO.Text                                as Text

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types              as Auth
import           Fission.User.Username.Types             as Username

data RoutesV2 mode = RoutesV2
  { append ::
      mode
      :- Summary     "Append a file"
      :> Description "Append a file to an app's public upload directory"
      --
      :> Capture    "Username"  Username
      :> Capture    "Creator"   Username
      :> Capture    "App Name"  Text
      :> Capture    "File Name" Text
      --
      :> ReqBody '[OctetStream] Serialized
      --
      :> Auth.HigherOrder
      :> PutNoContent
  }
  deriving Generic