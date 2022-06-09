module Fission.Web.API.Append.Types (RoutesV2 (..)) where

import           Network.IPFS.File.Types                 as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types              as Auth

import           Fission.FileSystem.DirectoryName.Types  as DirectoryName
import           Fission.FileSystem.FileName.Types       as FileName

newtype RoutesV2 mode = RoutesV2
  { append ::
      mode
      :- Summary     "Append a file"
      :> Description "Append a file to an app's public upload directory"
      --
      :> Capture    "App Name"  DirectoryName
      :> Capture    "File Name" FileName
      --
      :> ReqBody '[OctetStream] Serialized
      --
      :> Auth.HigherOrder
      :> PutNoContent
  }
  deriving Generic