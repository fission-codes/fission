module Fission.IPFS.Peer
  ( Peer (..)
  , all
  ) where

import           RIO hiding (all)
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Data.Has

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Process  as IPFSProc

import Fission.Config
import Fission.Internal.Constraint
import Fission.IPFS.Types

all :: (MonadRIO cfg m, Has IPFSPath cfg) => m (Either UnicodeException [Peer])
all = do
  allRaw <- rawList

  let
    textOrErr      = UTF8.encode allRaw
    peerNamesOrErr = Text.lines <$> textOrErr

  return $ fmap Peer <$> peerNamesOrErr

rawList :: (MonadRIO cfg m, Has IPFSPath cfg) => m Lazy.ByteString
rawList = IPFSProc.run' ["bootstrap", "list"]
