-- | Application monitoring
module Fission.Monitor
  ( Config (..)
  , wai
  ) where

import RIO

import System.Envy
import System.Remote.Monitoring.Wai

import Fission.Monitor.Types

wai :: HasLogFunc env => RIO env ()
wai = liftIO (decodeEnv :: IO (Either String Config)) >>= \case
  Left err -> do
    logError $ displayShow err
    Config { ekgHost, ekgPort } <- return defConfig
    liftIO . void $ forkServer ekgHost ekgPort

  Right Config { ekgHost, ekgPort } -> do
    logInfo $ mconcat
      [ "EKG available at "
      , displayBytesUtf8 ekgHost
      , ":"
      , display ekgPort
      , "\n"
      ]

    liftIO . void $ forkServer ekgHost ekgPort
