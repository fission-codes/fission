-- | Top level for all CLI config types
module Fission.CLI.Config.Types (Command (..)) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer.Lazy
import           Options.Applicative as OA

import           Fission.Prelude

-- -- | The action to attach to the command interface and description
-- newtype Command m = Command
--   { unCommand :: ExceptT (m ()) (Writer (Mod CommandFields (m ()))) () }
--   deriving newtype (Functor, Applicative, Monad)

-- data CLI = CLI
--   { version    :: SemVer
--   , summary    :: Text
--   , descripton :: Text
--   }

data Command input output = Command
  { command     :: Text
  , description :: Text
  , handler     :: input -> output
  }

class Monad m => MonadCommand m input where
  addCommand :: Command input output -> m output
