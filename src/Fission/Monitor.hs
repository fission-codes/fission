{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Fission.Monitor
  ( Config (..)
  , wai
  ) where

import RIO

import System.Envy
import System.Remote.Monitoring.Wai

data Config = Config
  { ekgHost :: ByteString
  , ekgPort :: Int
  } deriving (Generic, Show)

instance DefConfig Config where
  defConfig = Config "localhost" 9630

instance FromEnv Config

wai :: HasLogFunc env => RIO env ()
wai = liftIO (decodeEnv :: IO (Either String Config)) >>= \case
  Right Config { .. } -> do
    logInfo $ "EKG available at " <> displayBytesUtf8 ekgHost <> ":" <> display ekgPort <> "\n"
    liftIO $ void $ forkServer ekgHost ekgPort

  Left err -> do
    logError $ displayShow err
    Config { .. } <- return defConfig
    liftIO $ void $ forkServer ekgHost ekgPort
