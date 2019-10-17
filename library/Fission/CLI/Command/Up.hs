-- | File sync, IPFS-style
module Fission.CLI.Command.Up (command, up) where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           System.FilePath

import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help, value)

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.CLI.Auth    as Auth
import qualified Fission.CLI.Pin     as CLI.Pin
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
   => String
   -> m ()
up dir = do
  curr <- getCurrentDirectory
  let dir' = if isAbsolute dir then dir else curr </> dir
  logDebug "Starting single IPFS add locally of"

  IPFS.addDir dir' >>= \case
    Right cid -> Auth.withAuth (void . CLI.Pin.run cid)
    Left  err -> logError $ displayShow err
