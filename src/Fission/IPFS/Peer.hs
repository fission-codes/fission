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
all = nCode allRaw (fmap Peer . Text.lines)

allRaw :: MonadIO m => m Lazy.ByteString
allRaw = run ["bootstrap", "list"]

nCode :: Monad m => m Lazy.ByteString -> (Text -> a) -> m a
nCode p wrapper = UTF8.encode <$> p >>= \case
  Left err   -> error $ show err -- FIXME
  Right text -> return $ wrapper text

test :: MonadIO m => m Text
test = nCode testhash id

testhash :: MonadIO m => m Lazy.ByteString
testhash = run ["add", "./test.txt"]
-- testhash = run ["cat", "/ipfs/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG/readme"]
-- testhash = run ["cat", "/ipfs/QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ/cat.jpg"]

run :: MonadIO m => [String] -> m Lazy.ByteString
run = readProcessStdout_
    . setStdin createPipe
    . setStdout byteStringOutput
    . proc "/usr/local/bin/ipfs"
