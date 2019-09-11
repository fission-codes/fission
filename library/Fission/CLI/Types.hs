module Fission.CLI.Types (CommandM) where

import Options.Applicative as OA

import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy

type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()
