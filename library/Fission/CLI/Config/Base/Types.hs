-- | General configuration required to run any CLI function
module Fission.CLI.Config.Base.Types
  ( BaseConfig (..)
  , FissionBase (..)
  ) where

import           Fission.Prelude

import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Orphans ()

import           Fission.Web.Client
import qualified Fission.Web.Client.Types as Client

import           Network.IPFS
import qualified Network.IPFS.Types       as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

-- | The configuration used for the CLI application
data BaseConfig = BaseConfig
  { fissionAPI  :: !Client.Runner
  , logFunc     :: !LogFunc
  , processCtx  :: !ProcessContext
  , ipfsPath    :: !IPFS.BinPath
  , ipfsTimeout :: !IPFS.Timeout
  }

instance HasProcessContext BaseConfig where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc BaseConfig where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

-- | The top-level app type
newtype FissionBase a = FissionBase { unwrapFissionBase :: RIO BaseConfig a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader BaseConfig
                   , MonadThrow
                   )

instance MonadLogger FissionBase where
  monadLoggerLog loc src lvl msg = FissionBase (monadLoggerLog loc src lvl msg)

instance MonadWebClient FissionBase where
  run cmd = do
    Client.Runner runner <- asks fissionAPI
    liftIO <| runner cmd

instance MonadLocalIPFS FissionBase where
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
