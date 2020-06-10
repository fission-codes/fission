module Fission.CLI.Command.Types
  ( Command (..)
  , Leaf
  ) where

import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Writer.Lazy as Lazy

import           Options.Applicative.Simple      hiding (command)

import           Fission.Prelude

data Command m input output = Command
  { command     :: !Text
  , description :: !Text
  , argParser   :: !(Parser input)
  , handler     :: !(input -> m output)
  }

type Leaf = ExceptT (IO ()) (Lazy.Writer (Mod CommandFields (IO ()))) ()
