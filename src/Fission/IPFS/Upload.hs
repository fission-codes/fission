{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.IPFS.Upload where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Aeson
import Data.Aeson.TH
import System.Process.Typed

import qualified Fission.Internal.UTF8 as UTF8

data Blob = Blob { blob :: Text }
$(deriveJSON defaultOptions ''Blob)

insert :: MonadIO m => Lazy.ByteString -> m Lazy.ByteString
insert bs = run bs ["add"]

run :: MonadIO m => Lazy.ByteString -> [String] -> m Lazy.ByteString
run bs = readProcessStdout_
    . setStdin (byteStringInput bs)
    . setStdout byteStringOutput
    . proc "/usr/local/bin/ipfs"

nCode :: Monad m => m Lazy.ByteString -> (Text -> a) -> m a
nCode p wrapper = UTF8.encode <$> p >>= \case
  Left err   -> error $ show err -- FIXME
  Right text -> return $ wrapper text

test :: MonadIO m => Text -> m Text
test text = nCode (insert $ Lazy.fromStrict $ encodeUtf8 text) id
