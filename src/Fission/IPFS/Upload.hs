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

import Fission.Internal.Constraint

import qualified Fission.Internal.UTF8 as UTF8

data Blob = Blob { blob :: Text }
$(deriveJSON defaultOptions ''Blob)

insert :: MonadIO m => Lazy.ByteString -> m Lazy.ByteString
insert = run ["add"]

run :: MonadIO m => [String] -> Lazy.ByteString -> m Lazy.ByteString
run opts bs = readProcessStdout_
    . setStdin (byteStringInput bs)
    . setStdout byteStringOutput
    $ proc "/usr/local/bin/ipfs" opts

nCode :: Monad m => m Lazy.ByteString -> (Text -> a) -> m a
nCode p wrapper = UTF8.encode <$> p >>= \case
  Left err   -> error $ show err -- FIXME
  Right text -> return $ wrapper text

test :: (WithRIO env m, Loggable env) => Text -> m Text
test text = do
  logInfo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% here"
  (flip nCode) id (insert . Lazy.fromStrict $ encodeUtf8 text)
