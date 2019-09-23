module Fission.Web.Types
  ( Host (..)
  ) where

import RIO

import           Data.Aeson
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
