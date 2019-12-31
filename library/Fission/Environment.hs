-- | Environment variables
module Fission.Environment
  ( withFlag
  , withEnv
  , getFlag
  , getEnv
  , getFlagWithDefault
  , (.!~)
  ) where

import RIO.Char (toLower)
import System.Environment (lookupEnv)
import System.Envy

import Fission.Prelude
import Fission.Internal.Bool

-- | Get an environment variable. 'error's if not found.
getEnv :: FromEnv a => IO a
getEnv = decodeEnv >>= \case
  Left  msg -> error msg
  Right val -> pure val

-- | Fallback value for a monadic lookup
--
-- >>> Right (Just 9) .!~ 42
-- Right 9
--
-- >>> Right Nothing .!~ 42
-- Right 42
--
-- >>> Left (Just 9) .!~ 42
-- Left (Just 9)
--
-- >>> Left Nothing .!~ 42
-- Left Nothing
(.!~) :: Monad m => m (Maybe a) -> a -> m a
mVal .!~ fallback = pure (fromMaybe fallback) <*> mVal

-- | Switch on an environment flag
--
-- >>> withFlag "DEBUG" "nope" "yep"
-- "nope"
withFlag :: String -> a -> a -> IO a
withFlag key whenFalse whenTrue = withEnv key whenFalse (\_ -> whenTrue)

-- | Perform actions on an environment variable, with fallback if not available
--
-- >>> withEnv "HOST" "my.host" (drop 1)
-- "my.host"
withEnv :: String -> a -> (String -> a) -> IO a
withEnv key fallback transform = pure (maybe fallback transform) <*> lookupEnv key

-- | Check if an environment flag is set to 'True' (case-insensitive)
--
-- >>> getFlag "THIS_KEY_IS_UNSET"
-- Nothing
getFlag :: String -> IO (Maybe Bool)
getFlag key = do
  mayStr <- lookupEnv key
  let mayVal = truthy . fmap toLower <$> mayStr
  return mayVal

-- | Get a given flag, default to the given value if not found
getFlagWithDefault :: String -> Bool -> IO Bool
getFlagWithDefault flag defaultValue = getFlag flag .!~ defaultValue
