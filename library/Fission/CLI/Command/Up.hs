-- | File sync, IPFS-style
module Fission.CLI.Command.Up (cmd, up) where

import           Options.Applicative
import           RIO.Directory

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS

import           Fission.Prelude
import           Fission.Web.Client as Client

import           Fission.CLI.Command.Up.Types as Up
import qualified Fission.CLI.Prompt.BuildDir  as Prompt
import qualified Fission.CLI.IPFS.Pin         as CLI.Pin
import qualified Fission.CLI.DNS              as CLI.DNS
import           Fission.CLI.Display.Error

import           Fission.CLI.Environment
import           Fission.CLI.Command.Types
 
import qualified Fission.URL.DomainName.Types as URL
 
import qualified Fission.Web.Client.IPFS as IPFS

-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  )
  => Command m Up.Options ()
cmd = Command
  { command     = "up"
  , description = "Keep your current working directory up"
  , argParser   = parseOptions
  , handler     = up
  }

-- | Sync the current working directory to the server over IPFS
up ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  )
  => Up.Options
  -> m ()
up Up.Options {..} = do
  ignoredFiles <- getIgnoredFiles
  toAdd        <- Prompt.checkBuildDir path
  absPath      <- liftIO (makeAbsolute toAdd)

  logDebug $ "Starting single IPFS add locally of " <> displayShow absPath
  IPFS.addDir ignoredFiles absPath >>= putErrOr \cid -> do
    unless dnsOnly $
      CLI.Pin.add cid >>= putErrOr \_ -> noop

    CLI.DNS.update cid >>= putErrOr \_ -> noop

parseOptions :: Parser Up.Options
parseOptions = do
  dnsOnly <- switch $ mconcat
    [ long "dns-only"
    , help "Only update DNS (skip file sync)"
    ]

  path <- strArgument $ mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to sync"
    , value   "./"
    ]

  return Up.Options {..}
