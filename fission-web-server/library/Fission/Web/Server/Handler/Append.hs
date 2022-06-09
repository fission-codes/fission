module Fission.Web.Server.Handler.Append (handlerV2) where

import           Network.IPFS.File.Types                            as File
import           Network.IPFS.Local.Class (MonadLocalIPFS)
import           Network.IPFS.Remote.Class (MonadRemoteIPFS)

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.Append.Types                       as Append

import           Fission.Web.Server.App.Modifier                    as App
import           Fission.Web.Server.App.Retriever.Class (Retriever)
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Config.Types
import           Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.MonadDB.Class (MonadDB(..))


handlerV2 ::
  ( Modifier t
  , MonadRemoteIPFS m
  , MonadDB t m
  , MonadLogger m
  , MonadReader Config m
  , MonadTime m
  , MonadThrow m
  , Retriever m
  )
  => Append.RoutesV2 (AsServerT m)
handlerV2 = Append.RoutesV2 {append}
  where
    append appName fileName (Serialized rawData) Authorization {about = Entity userId _} = do
      Web.Err.ensureM $ App.addFile userId appName fileName rawData
      return NoContent
