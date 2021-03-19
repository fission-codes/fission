-- | Environment variables
module Fission.Environment
  ( withFlag
  , withEnv
  , getFlag
  , getEnv
  , getFlagWithDefault
  , (.!~)
  ) where

import           RIO.Char              (toLower)
import           System.Environment    (lookupEnv)
import           System.Envy

import           Fission.Internal.Bool
import           Fission.Prelude

-- | Get an environment variable. 'error's if not found.
getEnv :: FromEnv a => IO a
getEnv = decodeEnv >>= \case
  Left  msg -> error msg
  Right val -> pure val

-- | Fallback value for a monadic lookup
(.!~) :: Monad m => m (Maybe a) -> a -> m a
mVal .!~ fallback = pure (fromMaybe fallback) <*> mVal

-- | Switch on an environment flag
withFlag :: String -> a -> a -> IO a
withFlag key whenFalse whenTrue = withEnv key whenFalse (\_ -> whenTrue)

-- | Perform actions on an environment variable, with fallback if not available
withEnv :: String -> a -> (String -> a) -> IO a
withEnv key fallback transform = pure (maybe fallback transform) <*> lookupEnv key

-- | Check if an environment flag is set to 'True' (case-insensitive)
getFlag :: String -> IO (Maybe Bool)
getFlag key = do
  mayStr <- lookupEnv key
  let mayVal = truthy . fmap toLower <$> mayStr
  return mayVal

-- | Get a given flag, default to the given value if not found
getFlagWithDefault :: String -> Bool -> IO Bool
getFlagWithDefault flag defaultValue = getFlag flag .!~ defaultValue
