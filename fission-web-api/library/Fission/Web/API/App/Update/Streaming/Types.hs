module Fission.Web.API.App.Update.Streaming.Types where

import           Servant.API
import           Servant.Types.SourceT

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.URL.Types

import qualified Fission.Web.API.Auth.Types as Auth
import           Fission.Web.API.Prelude

type StreamingUpdate m
  = Summary "" -- FIXME
  :> Description "" -- FIXME
  --
  :> Capture    "App URL"   URL
  :> Capture    "New CID"   IPFS.CID
  --
  :> Auth.HigherOrder
  -- :> Stream 'PATCH 200 NewlineFraming JSON (SourceIO Natural) -- FIXME better type
  :> Stream 'PATCH 200 NewlineFraming JSON (SourceT m Natural) -- FIXME better type
