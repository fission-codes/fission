module Fission.CLI.Parser.Command.App.Up.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.IPFS.Types as IPFS
import           Fission.CLI.Parser.Open.Types
import           Fission.CLI.Parser.Watch.Types

data Options = Options
  { open       :: OpenFlag
  , watch      :: WatchFlag
  , updateDNS  :: Bool
  , updateData :: Bool
  , filePath   :: FilePath
  , ipfsCfg    :: IPFS.Config
  } deriving (Show, Eq)
