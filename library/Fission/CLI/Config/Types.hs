-- | Top level for all CLI config types
module Fission.CLI.Config.Types (CommandM) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer.Lazy
import           Options.Applicative             as OA

-- | The action to attach to the command interface and description
type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()
