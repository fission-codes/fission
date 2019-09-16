module Fission.CLI.Types
  ( CommandM
  , Config (..)
  , fissionAPI
  , logFunc
  , watchMgr
  ) where

import RIO

import Data.Has
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA
import System.FSNotify

import qualified Fission.Web.Client.Types as Client

-- | The action to attach to the command interface and description
type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

-- | The configuration used for the CLI application
data Config = Config
  { _fissionAPI :: !Client.Runner
  , _logFunc    :: !LogFunc
  , _watchMgr   :: !WatchManager
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has Client.Runner Config where
  hasLens = fissionAPI

instance Has WatchManager Config where
  hasLens = watchMgr
