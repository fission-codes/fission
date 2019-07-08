-- | Type for application monitoring
module Fission.Monitor.Types (Config (..)) where

import RIO

import System.Envy

data Config = Config
  { ekgHost :: ByteString
  , ekgPort :: Int
  } deriving ( Generic
             , FromEnv
             , Show
             )

instance DefConfig Config where
  defConfig = Config "localhost" 9630

-- instance FromEnv Config
