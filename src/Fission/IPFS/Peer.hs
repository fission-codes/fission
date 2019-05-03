{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
all = UTF8.encode <$> allRaw >>= \case
  Left err   -> error $ show err -- FIXME
  Right text -> return $ Peer <$> Text.lines text

allRaw :: MonadIO m => m Lazy.ByteString
allRaw =
  readProcessStdout_
  . setStdin createPipe
  . setStdout byteStringOutput
  $ proc "/usr/local/bin/ipfs" ["bootstrap", "list"]
