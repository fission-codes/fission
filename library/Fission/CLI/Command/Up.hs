-- | File sync, IPFS-style
module Fission.CLI.Command.Up (command, up) where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import           RIO.Process (HasProcessContext)

import           Data.Has

import           Options.Applicative.Simple hiding (command)

import           Fission.Internal.Constraint
import           Fission.Internal.Exception

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import           Fission.CLI.Command.Up.Types
import qualified Fission.CLI.Display.Error as Error
import qualified Fission.CLI.Auth          as Auth
import qualified Fission.CLI.Pin           as CLI.Pin
import qualified Fission.CLI.DNS           as CLI.DNS
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
    commandOptionsParser


commandOptionsParser :: Parser CommandOptions
commandOptionsParser = do
  optDnsOnly <- dnsOnly
  optLocation <- location

  pure CommandOptions {..}

  where
    dnsOnly :: Parser Bool
    dnsOnly =
      [ long "dns-only"
      , help "Don't upload anything to Fission"
      ]
      & mconcat
      & flag False True

    location :: Parser String
    location =
      [ metavar "Location"
      , help    "The location of the assets you want to upload"
      , value   "./"
      ]
      & mconcat
      & strArgument


-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => CommandOptions
   -> m ()
up options = handleWith_ Error.put' do
  let dir = optLocation options
  let dnsOnly = optDnsOnly options

  currDir       <- getCurrentDirectory
  cid           <- liftE . IPFS.addDir $ if isAbsolute dir
                                           then dir
                                           else currDir </> dir

  logDebug "Starting single IPFS add locally of"

  if dnsOnly
    then
      liftE . Auth.withAuth $ CLI.DNS.update cid
    else do
      liftE . Auth.withAuth $ CLI.Pin.run cid
      liftE . Auth.withAuth $ CLI.DNS.update cid
