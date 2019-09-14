module Fission.CLI.Types
  ( CommandM
  , Config (..)
  ) where

import RIO

import Data.Has
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA

import qualified Fission.Web.Client.Types as Client

type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

data Config = Config
  { _fissionAPI :: !Client.Runner
  , _logFunc    :: !LogFunc
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has Client.Runner Config where
  hasLens = fissionAPI
