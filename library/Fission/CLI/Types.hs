module Fission.CLI.Types
  ( ClientRunner (..)
  , CommandM
  , Config (..)
  ) where

import RIO

import Data.Has
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA
import Servant.Client

type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

newtype ClientRunner = ClientRunner
  { getRunner :: forall a. ClientM a -> IO (Either ServantError a) }

data Config = Config
  { _fissionAPI :: !ClientRunner
  , _logFunc    :: !LogFunc
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has ClientRunner Config where
  hasLens = fissionAPI
