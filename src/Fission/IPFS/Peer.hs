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

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFSProc

import Fission.Env
import Fission.Internal.Constraint

data Peer = Peer { peer :: Text }
$(deriveJSON defaultOptions ''Peer)

all :: (WithRIO env m, HasIPFSBin env) => m [Peer]
all = nCode allRaw (fmap Peer . Text.lines)

allRaw :: (WithRIO env m, HasIPFSBin env) => m Lazy.ByteString
allRaw = IPFSProc.run' ["bootstrap", "list"]

nCode :: Monad m => m Lazy.ByteString -> (Text -> a) -> m a
nCode p wrapper = UTF8.encode <$> p >>= \case
  Left err   -> error $ show err -- FIXME
  Right text -> return $ wrapper text
