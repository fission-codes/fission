-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import RIO

import Data.Has

import Servant
import Servant.Client

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.DNS.Client  as DNS.Client

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import qualified Fission.AWS.Types as AWS

update :: MonadRIO          cfg m
    => HasLogFunc        cfg
    => Has Client.Runner cfg
    => CID
    -> BasicAuthData
    -> m (Either ClientError AWS.DomainName)
update cid@(CID hash) auth = do
  logDebug $ "Updating DNS to " <> display hash

  Client.Runner runner <- Config.get
  update' runner auth cid >>= \case
    Right domain -> do
      CLI.Success.dnsUpdated $ AWS.getDomainName domain
      return $ Right domain

    Left err -> do
      CLI.Error.put' err
      return $ Left err

update' :: MonadIO m
        => (ClientM AWS.DomainName -> IO a)
        -> BasicAuthData
        -> CID
        -> m a
update' runner auth cid =
  liftIO . CLI.withLoader 50000
         . runner
         $ DNS.Client.update auth cid
