-- | File sync, IPFS-style
-- module Fission.CLI.Up (command, up) where
module Fission.CLI.Up where

import           RIO
import qualified RIO.Text as Text

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI
import           Turtle hiding ((<&>), err)

import           Servant
import           Servant.Client

import           Fission.Internal.Applicative -- TODO delete me!
import           Fission.Internal.Constraint

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types
import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Loader
import           Fission.CLI.Types

-- | The command to attach to the CLI tree
command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (const $ runRIO cfg up)
    noop

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => Has Client.Runner cfg
   => m ()
up = do
  logDebug "Starting single IPFS add locally"
  dir <- pwd
  Client.Runner runner <- Config.get
  Auth.withAuth \auth ->
    addDir dir \cid ->
      void $ up' auth runner cid

up' :: MonadRIO cfg m
    => HasLogFunc cfg
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
      void $ strict reconnect
      pinner runner auth cid >>= \case
        Right _ -> do
          putSuccess hash
          return $ Right cid
        Left err -> do
          putError err
          return $ Left err

addDir :: (MonadRIO cfg m, HasLogFunc cfg) => Turtle.FilePath -> (CID -> m ()) -> m ()
addDir dir action =
  case addDir' dir of
    Left err ->
      logError $ display err
    Right out -> do
      hash <- strict out
      action . CID $ Text.stripEnd hash

-- | Add the current working directory to IPFS locally
addDir' :: Turtle.FilePath -> Either Text (Shell Line)
addDir' dir = recursiveAdd <$> toText dir

recursiveAdd :: Text -> Shell Line
recursiveAdd txtPath = inproc "ipfs" ["add", "-HQr", txtPath] (pure "")

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

reconnect :: Shell Line
reconnect = inproc "ipfs" ["swarm", "connect", "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"] (pure "")
