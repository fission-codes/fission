module Fission.Environment
  ( withFlag
  , withEnv
  , getFlag
  , getEnv
  , (.!~)
  ) where

import RIO
import RIO.Char (toLower)

import System.Environment (lookupEnv)
import System.Envy

getEnv :: FromEnv a => IO a
getEnv = decodeEnv >>= \case
  Left msg  -> error msg
  Right val -> return val

-- | Fallback value for a monadic lookup
--
-- >>> Right (Just 9) .!~ 42
-- Right 9
--
-- >>> Right Nothing .!~ 42
-- Right 42
--
-- >>> Left (Just 9) .!~ 42
-- Left 9
(.!~) :: Monad m => m (Maybe a) -> a -> m a
mVal .!~ fallback = pure (fromMaybe fallback) <*> mVal

withFlag :: String -> a -> a -> IO a
withFlag key whenFalse whenTrue = withEnv key whenFalse (const whenTrue)

withEnv :: String -> a -> (String -> a) -> IO a
withEnv key fallback transform = pure (maybe fallback transform) <*> lookupEnv key

-- | Check if an environment flag is set to 'True' (case-insensitive)
getFlag :: String -> IO Bool
getFlag key =
  pure (maybe False $ \flag -> fmap toLower flag == "true") <*> lookupEnv key
