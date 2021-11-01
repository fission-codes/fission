module Network.IPFS.URL.Types (URL (..)) where

import qualified Servant.Client as Client

import           Network.IPFS.Prelude

-- | IPFS client URL
newtype URL = URL { getURL :: Client.BaseUrl }
  deriving         ( Eq
                   , Generic
                   , Show
                   )
  deriving newtype ( FromJSON )
