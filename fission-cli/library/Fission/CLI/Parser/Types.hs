module Fission.CLI.Parser.Types (Options (..)) where

import qualified RIO.Text                         as Text

import           Fission.Prelude

import           Fission.User.DID.Types

import           Fission.CLI.Parser.Command.Types
import           Fission.CLI.Remote.Types

data Options = Options
  { cmd        :: Command   -- ^ The actual command
  , remote     :: Remote    -- ^ Remote environment
  , fissionDID :: Maybe DID -- ^ DID of remote server
  } deriving (Eq, Show)

instance Display Options where
  textDisplay = Text.pack . show
