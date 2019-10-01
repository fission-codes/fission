module Fission.CLI.Config.Types
  ( CommandM
  , Config (..)
  , fissionAPI
  , logFunc
  ) where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))

import Data.Has
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS

-- | The action to attach to the command interface and description
type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

-- | The configuration used for the CLI application
data Config = Config
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _processCtx  :: !ProcessContext
  , _ipfsPath    :: !IPFS.BinPath
  , _ipfsTimeout :: !IPFS.Timeout
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has Client.Runner Config where
  hasLens = fissionAPI

instance HasProcessContext Config where
  processContextL = processCtx

instance Has IPFS.BinPath Config where
  hasLens = ipfsPath

instance Has IPFS.Timeout Config where
  hasLens = ipfsTimeout
