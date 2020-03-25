module Fission.CLI.Config.Connected.Types
  ( ConnectedConfig (..)
  , FissionConnected (..)
  ) where

import           Control.Monad.Catch

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.Client.Types as Client

import           Fission.CLI.Environment.Class

data ConnectedConfig = ConnectedConfig
  { fissionAPI   :: !Client.Runner
  , logFunc      :: !LogFunc
  , processCtx   :: !ProcessContext
  , ipfsPath     :: !IPFS.BinPath
  , ipfsTimeout  :: !IPFS.Timeout
  , peer         :: !IPFS.Peer
  , ignoredFiles :: !IPFS.Ignored
  }

instance HasProcessContext ConnectedConfig where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc ConnectedConfig where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

-- | The top-level app type
newtype FissionConnected a = FissionConnected { unwrapFissionConnected :: RIO ConnectedConfig a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader ConnectedConfig
                   , MonadThrow
                   , MonadCatch
                   )

instance MonadLogger FissionConnected where
  monadLoggerLog loc src lvl msg = FissionConnected (monadLoggerLog loc src lvl msg)

instance MonadWebClient FissionConnected where
  run cmd = do
    Client.Runner runner <- asks fissionAPI
    liftIO <| runner cmd

instance MonadLocalIPFS FissionConnected where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks ipfsPath
    IPFS.Timeout secs <- asks ipfsTimeout

    let opts' = ("--timeout=" <> show secs <> "s") : opts

    runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
      (ExitSuccess, contents, _) ->
        return <| Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            return . Left <| Process.Timeout secs
        | otherwise ->
            return . Left <| Process.UnknownErr stdErr

instance MonadEnvironment FissionConnected where
  getIgnoredFiles = asks ignoredFiles
