module Fission.Web.Server.Handler.Append (handlerV2) where

import           Network.IPFS.File.Types                as File

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.Append.Types           as Append

import           Fission.Web.Server.Authorization.Types



handlerV2 ::
  ( MonadLogger m
  )
  => Append.RoutesV2 (AsServerT m)
handlerV2 = Append.RoutesV2 {append}
  where
    append username creator appName fileName (Serialized rawData) Authorization {about = Entity userId _} =
      return NoContent
