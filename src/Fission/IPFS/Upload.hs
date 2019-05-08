{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.IPFS.Upload where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Bifunctor as Bi

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFS.Proc

import Fission.Internal.Constraint
import Fission.Env

run :: (WithRIO env m, HasIPFSPath env) => Text -> m (Either UnicodeException Text)
run = fmap (Bi.second $ Text.dropSuffix "\n")
    . fmap UTF8.encode
    . IPFS.Proc.run ["add", "-q"]
    . Lazy.fromStrict
    . encodeUtf8
