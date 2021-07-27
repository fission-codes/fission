module Fission.Web.API.App.Update.Streaming.Types where

import qualified Network.IPFS.CID.Types      as IPFS
import           Servant.API

import           Fission.BytesReceived.Types
import           Fission.URL.Types
import qualified Fission.Web.API.Auth.Types  as Auth

type StreamingUpdate
  =  Summary     "Set app content & stream upload progress"
  :> Description "Update the content (CID) of an app & stream the progress"
  --
  :> Capture "App URL" URL
  :> Capture "New CID" IPFS.CID
  --
  :> Auth.HigherOrder
  :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO BytesReceived)
