module Fission.Web.Types
  ( Host (..)
  , Port (..)
  ) where

import RIO

import Network.Wai.Handler.Warp as Warp (Port)
import System.Envy

newtype Host = Host { getHost :: Text }
  deriving (Show, IsString)

newtype Port = Port { port :: Warp.Port }
  deriving (Show, IsString)

instance FromEnv Host where
  fromEnv = Host <$> env "HOST"

instance FromEnv Port where
  fromEnv = Port <$> envMaybe "PORT" .= 1337
