-- | Application logging types
module Fission.Log.Types (MinLevel (..)) where

import RIO
import RIO.Char (toLower)

import System.Envy

import qualified Fission.Internal.UTF8 as UTF8

-- | The minimum level to log at
newtype MinLevel = MinLevel { getLevel :: LogLevel }
  deriving (Eq, Show)

instance FromEnv MinLevel where
  fromEnv = do
    levelEnv <- env "MIN_LOG_LEVEL" .!= "debug"
    pure . MinLevel $ case toLower <$> levelEnv of
      "debug" -> LevelDebug
      "error" -> LevelError
      "info"  -> LevelInfo
      "warn"  -> LevelWarn
      other   -> LevelOther (UTF8.textShow (other :: String))
