module Network.IPFS.Client.Files
  ( API
  , module Statistics
  ) where

import           Servant.API

import qualified Network.IPFS.Client.Files.Copy.Types as Copy
import qualified Network.IPFS.Client.Files.Remove.Types as Remove
import qualified Network.IPFS.Client.Files.Statistics.Types as Statistics
import qualified Network.IPFS.Client.Files.Write.Types as Write


type API = Copy.API :<|> Remove.API :<|> Statistics.API :<|> Write.API
