module Fission.CLI.IPFS.Configure
  ( init
  , setBootstrap
  , setApiAddress
  , setGatewayAddress
  , setSwarmAddresses
  , enableRelay
  ) where

import qualified RIO.ByteString.Lazy          as Lazy

import           Network.IPFS.Local.Class     as IPFS
import qualified Network.IPFS.Process.Error   as IPFS
import qualified Network.IPFS.Types           as IPFS

import           Turtle

import           Fission.Prelude

import           Fission.CLI.Environment      hiding (init)
import           Fission.CLI.Environment.Path

init :: (MonadIO m, MonadEnvironment m) => m ExitCode
init = do
  IPFS.BinPath ipfsPath <- globalIPFSBin
  ipfsRepo              <- globalIPFSRepo
   -- Needs to be run manually because it's a prerequesite for the daemon
  runProcess . fromString $ intercalate " "
    [ "IPFS_PATH=" <> ipfsRepo
    , ipfsPath
    , "init"
    , "&> /dev/null"
    ]

setBootstrap :: forall m . MonadLocalIPFS m => m (Either IPFS.Error ())
setBootstrap =
  fmap sequence results >>= \case
    Right _    -> pure $ Right ()
    Left  err' -> pure $ Left err'

  where
    results :: m [Either IPFS.Error IPFS.RawMessage]
    results = forM peers \peer -> IPFS.runLocal ["bootstrap", "add"] peer

    peers :: [Lazy.ByteString]
    peers =
      [ "/dns4/node.runfission.com/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
      , "/ip4/3.226.224.78/tcp/4001/p2p/QmPeeeZZXxMBAPxxba7a6ggjDr5jLuD3RAFnmPPcvJ9fMS"
      , "/ip4/3.226.224.78/udp/4001/quic/p2p/QmPeeeZZXxMBAPxxba7a6ggjDr5jLuD3RAFnmPPcvJ9fMS"
      ]

setApiAddress ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
setApiAddress =
  ensureM $ IPFS.runLocal
    [ "config"
    , "Addresses.API"
    ]
    "/ip4/0.0.0.0/tcp/10235"

setGatewayAddress ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
setGatewayAddress =
  ensureM $ IPFS.runLocal
    [ "config"
    , "Addresses.Gateway"
    ]
    "/ip4/0.0.0.0/tcp/11235"

setSwarmAddresses ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRescue    m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
setSwarmAddresses = do
  raw                <- ensureM $ IPFS.runLocal ["config", "Addresses.Swarm"] ""
  existing :: [Text] <- case eitherDecode raw of
                          Left err' -> do
                            logDebug $ "Unable to parse `ipfs config Address.Swarm` response: " <> err'
                            return []

                          Right addrs ->
                            return addrs

  ensureM $ IPFS.runLocal
    [ "config --json"
    , "Addresses.Swarm"
    ]
    (encode ("/ip4/0.0.0.0/tcp/4231/wc" : existing))

enableRelay ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
enableRelay =
  ensureM $ IPFS.runLocal
    [ "config --bool"
    , "Swarm.EnableRelayHop"
    ]
    "true"
