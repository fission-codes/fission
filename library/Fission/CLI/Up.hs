-- | File sync, IPFS-style
module Fission.CLI.Up (command, up, up') where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI

import           Servant
import           Servant.Client

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types   as IPFS

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types
import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Loader
import           Fission.CLI.Types

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (const $ runRIO cfg up)
    (pure ())

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => m ()
up = do
  logDebug "Starting single IPFS add locally"
  dir <- getCurrentDirectory
  Client.Runner runner <- Config.get
  Auth.withAuth \auth ->
    IPFS.addDir dir >>= \case
      Left err  -> logError $ displayShow err
      Right cid -> void $ up' auth runner cid

up' :: MonadRIO          cfg m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => Show a
    => BasicAuthData
    -> (ClientM NoContent -> IO (Either a b))
    -> CID
    -> m (Either a CID)
up' auth runner cid@(CID hash) = do
  logDebug $ "Remote pinning " <> display hash
  pinner runner auth cid >>= \case
    Right _ -> do
      putSuccess hash
      return $ Right cid

    Left _ -> do
      logError "Failed to pin remotely, attempting to reconnect to IPFS"
      IPFS.Peer.connect IPFS.Peer.fission >> pinner runner auth cid >>= \case
        Right _ -> do
          putSuccess hash
          return $ Right cid
        Left err -> do
          putError err
          return $ Left err

pinner :: MonadIO m => (ClientM NoContent -> IO a) -> BasicAuthData -> CID -> m a
pinner runner auth cid =
  liftIO . withLoader 50000
         . runner
         $ Fission.pin (Fission.request auth) cid

putError :: (MonadRIO cfg m, HasLogFunc cfg, Show a) => a -> m ()
putError err = do
  logError $ displayShow err
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  putText $ mconcat
    [ Emoji.prohibited
    , " Something went wrong. Please try again or file a bug report with "
    , "Fission support at https://github.com/fission-suite/web-api/issues/new"
    ]
  liftIO $ ANSI.setSGR [ANSI.Reset]

putSuccess :: MonadIO m => Text -> m ()
putSuccess hash = do
  putText $ Emoji.rocket <> "Your current working directory is now live\n"
  putText $ Emoji.okHand <> hash  <> "\n"
