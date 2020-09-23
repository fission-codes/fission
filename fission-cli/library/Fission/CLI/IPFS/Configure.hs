module Fission.CLI.IPFS.Configure
  ( init
  , bootstrap
  , apiAddresses
  , gatewayAddresses
  ) where

import qualified RIO.ByteString.Lazy          as Lazy

import           Network.IPFS.Local.Class     as IPFS
import qualified Network.IPFS.Types           as IPFS

import           Turtle

import           Fission.Prelude

import           Fission.CLI.Environment      hiding (init)
import           Fission.CLI.Environment.Path

init :: (MonadIO m, MonadEnvironment m) => m ()
init = do
  IPFS.BinPath ipfsPath <- globalIPFSBin
  void . runProcess . fromString $ ipfsPath <> " init &> /dev/null"

bootstrap :: (MonadIO m, MonadLocalIPFS m) => m ()
bootstrap = forM_ peers (IPFS.runLocal ["bootstrap", "add"])
  where
    peers :: [Lazy.ByteString]
    peers =
      [ "/dns4/node.runfission.com/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
      , "/ip4/3.226.224.78/tcp/4001/p2p/QmPeeeZZXxMBAPxxba7a6ggjDr5jLuD3RAFnmPPcvJ9fMS"
      , "/ip4/3.226.224.78/udp/4001/quic/p2p/QmPeeeZZXxMBAPxxba7a6ggjDr5jLuD3RAFnmPPcvJ9fMS"
      ]

apiAddresses :: (MonadIO m, MonadEnvironment m) => m ()
apiAddresses = do
  IPFS.BinPath ipfsPath <- globalIPFSBin
  void . runProcess . fromString $ ipfsPath <> " config Addresses.API /ip4/0.0.0.0/tcp/10235"

gatewayAddresses :: (MonadIO m, MonadEnvironment m) => m ()
gatewayAddresses = do
  IPFS.BinPath ipfsPath <- globalIPFSBin
  void . runProcess . fromString $ ipfsPath <> " config Addresses.Gateway /ip4/0.0.0.0/tcp/11235"
