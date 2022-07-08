module Fission.Web.Server.Handler.Append (handlerV2) where

import           Fission.Web.Server.IPFS.DNSLink.Class
import           Network.IPFS.File.Types                            as File
import           Network.IPFS.Remote.Class (MonadRemoteIPFS)

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.Append.Types                       as Append

import           Fission.Web.Server.App.Domain.Retriever            as App.Domain
import           Fission.Web.Server.App.Modifier                    as App
import           Fission.Web.Server.App.Retriever.Class             as App
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Domain.Retriever.Class          as Domain
import           Fission.Web.Server.Config.Types
import           Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.MonadDB.Class (MonadDB(..))


handlerV2 ::
  ( App.Domain.Retriever m
  , App.Modifier t
  , App.Retriever m
  , Domain.Retriever m
  , MonadRemoteIPFS m
  , MonadDB t m
  , MonadDNSLink m
  , MonadLogger m
  , MonadReader Config m
  , MonadTime m
  )
  => Append.RoutesV2 (AsServerT m)
handlerV2 = Append.RoutesV2 {append}
  where
    append appName fileName (Serialized rawData) Authorization {about = Entity userId _} = do
      cid <- Web.Err.ensureM $ App.addFile userId appName fileName rawData
      return cid
