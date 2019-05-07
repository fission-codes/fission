{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.IPFS.Upload where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Aeson
import Data.Aeson.TH

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFSProc

data Blob = Blob { blob :: Text }
$(deriveJSON defaultOptions ''Blob)

run :: MonadIO m => Text -> m (Either UnicodeException Text)
run = fmap UTF8.encode
     . IPFSProc.run ["add"]
     . Lazy.fromStrict
     . encodeUtf8
