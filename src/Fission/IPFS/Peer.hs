{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.IPFS.Peer
  ( Peer (..)
  , peers
  , rawPeers
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Aeson
import Data.Aeson.TH
import Data.Has

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFSProc

import Fission.Config
import Fission.Internal.Constraint

data Peer = Peer { peer :: Text }
$(deriveJSON defaultOptions ''Peer)

peers :: (MonadRIO cfg m, Has IpfsPath cfg) => m (Either UnicodeException [Peer])
peers = do
  allRaw <- rawPeers

  let textOrErr      = UTF8.encode allRaw
      peerNamesOrErr = Text.lines <$> textOrErr

  fmap Peer <$> peerNamesOrErr

rawPeers :: (MonadRIO cfg m, Has IpfsPath cfg) => m Lazy.ByteString
rawPeers = IPFSProc.run' ["bootstrap", "list"]
