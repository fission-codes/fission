module Fission.IPFS.Peer
  ( all
  , rawList
  ) where

import           RIO hiding (all)
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Has

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFSProc

import Fission.Types
import Fission.Internal.Constraint
import qualified Fission.IPFS.Types as IPFS

all :: MonadRIO cfg m
    => Has IPFS.Path cfg
    => m (Either UnicodeException [IPFS.Peer])
all = do
  allRaw <- rawList

  let
    textOrErr      = UTF8.encode allRaw
    peerNamesOrErr = Text.lines <$> textOrErr

  return $ fmap IPFS.Peer <$> peerNamesOrErr

rawList :: (MonadRIO cfg m, Has IPFS.Path cfg) => m Lazy.ByteString
rawList = IPFSProc.run' ["bootstrap", "list"]
