-- | File sync, IPFS-style
module Fission.CLI.Command.Up
  ( cmd
  , up
  , requestWithRetry
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Options.Applicative
import           RIO.Directory

import           Network.HTTP.Types.Status

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS

import           Servant.Client

import           Fission.Prelude
import           Fission.Models

import           Fission.Error

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token
 
import           Fission.Web.Client     as Client
import           Fission.Web.Client.App as App

import           Fission.CLI.Environment as Environment

import           Fission.CLI.Command.Types
import           Fission.CLI.Command.Up.Types as Up
import qualified Fission.CLI.Command.App.Init as App.Init

import           Fission.CLI.Display.Error
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Success as CLI.Success
 
import qualified Fission.CLI.Prompt.BuildDir as Prompt

-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
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
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  )
  => Up.Options
  -> m ()
up Up.Options {..} = do
  ignoredFiles         <- getIgnoredFiles
  toAdd                <- Prompt.checkBuildDir path
  absPath              <- liftIO $ makeAbsolute toAdd
  Environment {appURL} <- Environment.get

  let copyFiles = not dnsOnly

  case appURL of
    Nothing ->
      CLI.Error.put (NotFound @App) $
        "You have not set up an app. Please run " <> App.Init.cmdTxt

    Just url -> do
      logDebug $ "Starting single IPFS add locally of " <> displayShow absPath
     
      IPFS.addDir ignoredFiles absPath >>= putErrOr \cid -> do
        retrier (updateApp url cid copyFiles) >>= \case
          Left err ->
            CLI.Error.put err "Server unable to sync data"
           
          Right _  -> do
            CLI.Success.live cid
            CLI.Success.dnsUpdated url

  where
    updateApp url cid copyFiles =
      authClient (Proxy @App.Update)
        `withPayload` url
        `withPayload` cid
        `withPayload` Just copyFiles

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

requestWithRetry ::
  ( MonadWebClient m
  , MonadLogger m
  )
  => m (ClientM a)
  -> m (Either ClientError a)
requestWithRetry req =
  sendRequestM req >>= \case
    Right val ->
      return $ Right val

    Left err@(FailureResponse _req res) -> do
      let code = statusCode $ responseStatusCode res

      if code >= 502 && code <= 504
        then do
          logWarn $ "Got a " <> textDisplay code <> "; retrying..."
          retrier req

        else
          return $ Left err

    Left err ->
      return $ Left err
