-- | Pin files via the CLI
module Fission.CLI.Pin
  ( pin
  , run
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has

import Servant
import Servant.Client

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types   as IPFS
import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

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
  IPFS.Peer.connect IPFS.Peer.fission

  Client.Runner runner <- Config.get
  liftIO (pin runner auth cid) >>= \case
    Right _ -> do
      CLI.Success.live hash
      return $ Right cid

    Left err -> do
      CLI.Error.put' err
      return $ Left err

pin :: MonadUnliftIO m => (ClientM NoContent -> m a) -> BasicAuthData -> CID -> m a
pin runner auth cid = CLI.withLoader 50000 . runner $ Fission.pin (Fission.request auth) cid
