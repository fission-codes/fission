module Fission.CLI.Parser.Types (Options (..)) where

import qualified RIO.Text                         as Text
import           Servant.Client

import           Fission.Prelude

import           Fission.CLI.Parser.Command.Types
import           Fission.User.DID.Types

data Options = Options
  { cmd        :: !Command     -- ^ The actual command
  , fissionURL :: !BaseUrl     -- ^ URL of remote server
  , fissionDID :: !(Maybe DID) -- ^ DID of remote server
  } deriving (Eq, Show)

instance Display Options where
  textDisplay = Text.pack . show
