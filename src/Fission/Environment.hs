module Fission.Environment
  ( Environment (..)
  , withFlag
  , withEnv
  , getFlag
  , getEnv
  , decodeElse
  ) where

import RIO
import RIO.Char (toLower)
import RIO.Text (pack)

import System.Environment (lookupEnv)
import System.Envy

data Environment
  = Test
  | Development
  --  | Staging
  | Production
  deriving (Eq, Show, Read)

instance Display Environment where
  display     = displayShow
  textDisplay = pack . show

getEnv :: FromEnv a => IO a
getEnv = decodeEnv >>= \case
  Left msg  -> error msg
  Right val -> return val

decodeElse :: FromEnv a => a -> IO a
decodeElse fallback = decodeEnv >>= \case
  Left  _   -> return fallback
  Right val -> return val

withFlag :: String -> a -> a -> IO a
withFlag key whenFalse whenTrue = withEnv key whenFalse (const whenTrue)

withEnv :: String -> a -> (String -> a) -> IO a
withEnv key fallback transform = pure . maybe fallback transform =<< lookupEnv key

getFlag :: String -> IO Bool
getFlag = pure . maybe False (\flag -> fmap toLower flag == "true") <=< lookupEnv
