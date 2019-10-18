-- | File sync, IPFS-style
module Fission.CLI.Command.Up (command, up) where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Control.Monad.Except

import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help, value)

import           Fission.Internal.Constraint
import           Fission.Internal.Exception

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.CLI.Error   as Error
import qualified Fission.CLI.Auth    as Auth
import qualified Fission.CLI.Pin     as CLI.Pin
import qualified Fission.CLI.DNS     as CLI.DNS
import           Fission.CLI.Config.Types

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
    (runRIO cfg . up)
    (strArgument $ metavar "Location" <> help "The location of the assets you want to upload" <> value "./")

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => m ()
up = runLogged do
  logDebug "Starting single IPFS add locally"

  dir <- liftIO getCurrentDirectory
  cid <- liftE $ IPFS.addDir dir

  liftE $ Auth.withAuth (CLI.Pin.run cid)
  liftE $ Auth.withAuth (CLI.DNS.update cid)

  return ()

-- liftE :: (Functor m, Error.ToError err) => m (Either err a) -> ExceptT Error.Error m a
-- liftE = ExceptT . fmap Error.eitherCLI

-- announceAnyErrors :: (MonadRIO cfg m, HasLogFunc cfg, Show err) => Either err a -> m (Either err a)
-- announceAnyErrors = \case
--   Left err -> do
--     logDebug $ displayShow err
--     return $ Left err

--   Right val ->
--     return $ Right val

-- runCLI :: (MonadIO m, MonadReader cfg m, HasLogFunc cfg, Show err) => ExceptT err m a -> m (Either err a)
-- runCLI = announceAnyErrors <=< runExceptT

-- runCLI_ :: (MonadIO f, MonadReader cfg f, HasLogFunc cfg, Show err) => ExceptT err f a -> f ()
-- runCLI_ = void . runCLI
