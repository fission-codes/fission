module Fission.Web.Types
  ( Host (..)
  , Port (..)
  ) where

import RIO

import           Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.Client
import           System.Envy

-- | The hostname of the running application
newtype Host = Host { getHost :: BaseUrl }
  deriving          ( Eq
                    , Show
                    )

instance FromJSON Host where
  parseJSON = withText "Web.Host" \txt ->
    Host <$> parseJSON (String txt)

-- | Port of the running application
newtype Port = Port { port :: Warp.Port }
  deriving          ( Show
                    , Eq
                    , Generic
                    )

instance FromEnv Port where
  fromEnv _ = Port <$> env "PORT"

instance FromJSON Port where
  parseJSON = withScientific "Web.Port" \num ->
    Port <$> parseJSON (Number num)
