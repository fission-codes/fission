{-# LANGUAGE TemplateHaskell #-}

module Fission.IPFS.Peer where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Aeson
import Data.Aeson.TH
import System.Process.Typed

import qualified Fission.Internal.UTF8 as UTF8

data Peer = Peer { peer :: Text }
$(deriveJSON defaultOptions ''Peer)

all :: MonadIO m => m [Peer]
all = rawAll >>= return . UTF8.encode >>= \case
  Left err   -> error $ show err
  Right text -> return $ Peer <$> Text.lines text

rawAll :: MonadIO m => m Lazy.ByteString
rawAll =
  readProcessStdout_
  . setStdin createPipe
  . setStdout byteStringOutput
  $ proc "/usr/local/bin/ipfs" ["bootstrap", "list"]
