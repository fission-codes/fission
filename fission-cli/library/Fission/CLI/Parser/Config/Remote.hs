module Fission.CLI.Parser.Config.Remote
  ( parser
  -- * Reexports
  , module Fission.CLI.Parser.Config.Remote.Types
  ) where

import           Options.Applicative

import           Fission.CLI.Parser.Config.Remote.Types
import qualified Fission.CLI.Parser.DID                 as DID
import qualified Fission.CLI.Parser.Remote              as Remote

parser :: Parser RemoteConfig
parser = do
  mayDID <- DID.parser
  target <- Remote.parser
  pure RemoteConfig {..}
