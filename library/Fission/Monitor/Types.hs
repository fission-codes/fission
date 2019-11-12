-- | Type for application monitoring
module Fission.Monitor.Types (Config (..)) where

import System.Envy

import Fission.Prelude

-- | Monitor configuration
data Config = Config
  { ekgHost :: ByteString -- ^ EKG host name
  , ekgPort :: Int        -- ^ EKG port
  } deriving ( Generic
             , FromEnv
             , Show
             )

instance DefConfig Config where
  defConfig = Config "localhost" 9630
