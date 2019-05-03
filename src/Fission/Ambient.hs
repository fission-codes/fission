{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Ambient where

import RIO
import System.Environment (lookupEnv)

lookup :: (Read a, MonadIO m) => String -> a -> m a
lookup key fallback = liftIO $ lookupEnv key >>= \case
  Nothing  -> return fallback
  Just raw ->
    case readMaybe raw of
      Nothing -> error $ "Failed to parse [[" <> raw <> "]] for environment variable " <> key
      Just t -> return t
