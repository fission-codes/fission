module Fission.Web.Types
  ( Host (..)
  , Port (..)
  ) where

import RIO

import           Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import           System.Envy

-- | The hostname of the running application
newtype Host = Host { getHost :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  IsString

instance FromEnv Host where
  fromEnv _ = Host <$> env "HOST"

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
