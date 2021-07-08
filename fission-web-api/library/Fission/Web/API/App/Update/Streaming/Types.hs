module Fission.Web.API.App.Update.Streaming.Types where

import           Servant.API
import           Servant.Types.SourceT

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.URL.Types

import qualified Fission.Web.API.Auth.Types as Auth
import           Fission.Web.API.Prelude





import           Servant.Types.SourceT      as S
import           Streamly.Prelude

type StreamingUpdate
  = Summary "" -- FIXME
  :> Description "" -- FIXME
  --
  :> Capture    "App URL"   URL
  :> Capture    "New CID"   IPFS.CID
  --
  :> Auth.HigherOrder
  -- :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO Natural) -- FIXME better type
  -- :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO Natural) -- FIXME better type
  :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO UploadStatus) -- FIXME better type


isUploading :: UploadStatus -> Bool
isUploading = \case
  Uploading _ -> True
  _           -> False


data UploadStatus
  = Failed
  | Uploading Natural
  | Done
