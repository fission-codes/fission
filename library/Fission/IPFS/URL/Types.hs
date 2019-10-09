module Fission.IPFS.URL.Types (URL (..)) where

import RIO

import           Data.Aeson
import qualified Servant.Client as Client

-- | IPFS client URL
newtype URL = URL { getURL :: Client.BaseUrl }
  deriving         ( Eq
                   , Generic
                   , Show
                   )
  deriving newtype ( FromJSON )
