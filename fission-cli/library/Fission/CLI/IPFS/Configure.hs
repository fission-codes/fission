module Fission.CLI.IPFS.Configure
  ( init
  , setBootstrap
  , setApiAddress
  , setGatewayAddress
  , setSwarmAddresses
  , enableRelay
  , enableHolePunching
  ) where

import qualified RIO.ByteString.Lazy        as Lazy

import           Network.IPFS.Local.Class   as IPFS
import qualified Network.IPFS.Process.Error as IPFS

import           Turtle

import           Fission.Prelude


init ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
init = do
  ensureM $ IPFS.runLocal ["init"] ""

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
    (fromString . show $ encode ("/ip4/0.0.0.0/tcp/4231/ws" : existing))
               -- `show` to get string escaping

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

enableHolePunching ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Error
  )
  => m IPFS.RawMessage
enableHolePunching =
  ensureM $ IPFS.runLocal
    [ "config --bool"
    , "Swarm.EnableHolePunching"
    ]
    "true"
