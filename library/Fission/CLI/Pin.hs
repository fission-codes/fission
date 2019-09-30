-- | Pin files via the CLI
module Fission.CLI.Pin
  ( pin
  , run
  ) where

import RIO
import RIO.Process   (HasProcessContext)

import Data.Has

import Servant
import Servant.Client

import Fission.Internal.Constraint

import qualified Fission.Config          as Config

import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types   as IPFS
import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission

import           Fission.CLI.Error   as CLI.Error
import           Fission.CLI.Success as CLI.Success
import qualified Fission.CLI.Loader  as CLI

run :: MonadRIO          cfg m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has Client.Runner cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => CID
    -> BasicAuthData
    -> m (Either ClientError CID)
run cid@(CID hash) auth = do
  logDebug $ "Remote pinning " <> display hash
  Client.Runner runner <- Config.get

  pin runner auth cid >>= \case
    Right _ ->
      success

    Left _ -> do
      logError "Failed to pin remotely, attempting to reconnect to IPFS"
      IPFS.Peer.connect IPFS.Peer.fission
      pin runner auth cid >>= \case
        Right _ ->
          success

        Left err -> do
          CLI.Error.put' err
          return $ Left err
  where
    success :: MonadIO m => m (Either err CID)
    success = do
      CLI.Success.live hash
      return $ Right cid

pin :: MonadIO m => (ClientM NoContent -> IO a) -> BasicAuthData -> CID -> m a
pin runner auth cid =
  liftIO . CLI.withLoader 50000
         . runner
         $ Fission.pin (Fission.request auth) cid
