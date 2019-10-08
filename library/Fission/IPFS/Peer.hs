module Fission.IPFS.Peer
  ( all
  , rawList
  , connect
  , fission
  , getExternalAddress
  ) where

import           RIO hiding (all)
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text
import qualified RIO.List            as List
import           RIO.Process (HasProcessContext)

import qualified Net.IPv4 as IPv4
import           Text.Regex

import           Data.Has
import qualified Data.Aeson as JSON

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFSProc
import qualified Fission.IPFS.Types          as IPFS
import           Fission.IPFS.Peer.Error     as IPFS.Peer
import           Fission.IPFS.Peer.Types
import           Fission.IPFS.Info.Types

all :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => m (Either IPFS.Peer.Error [IPFS.Peer])
all = rawList <&> \case
  (ExitSuccess, allRaw, _) ->
    case UTF8.encode allRaw of
      Left  _    -> Left . DecodeFailure $ show allRaw
      Right text -> Right $ IPFS.Peer <$> Text.lines text

  (ExitFailure _, _, err) ->
    Left . UnknownErr $ UTF8.textShow err

rawList :: MonadRIO          cfg m
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => m (ExitCode, Lazy.ByteString, Lazy.ByteString)
rawList = IPFSProc.run' ["bootstrap", "list"]

connect :: MonadRIO cfg m
        => HasProcessContext cfg
        => HasLogFunc cfg
        => Has IPFS.BinPath cfg
        => Has IPFS.Timeout cfg
        => Peer
        -> m (Either IPFS.Peer.Error ())
connect peer@(Peer peerID) = IPFSProc.run ["swarm", "connect"] (UTF8.textToLazyBS peerID) >>= pure . \case
  (ExitFailure _ , _, _) -> Left $ CannotConnect peer
  (ExitSuccess   , _, _) -> Right ()

peerAddressRe :: Regex
peerAddressRe = mkRegex "^/ip[46]/([a-zA-Z0-9.:]*)/"

-- | Retrieve just the ip address from a peer address
extractIPfromPeerAddress :: String -> Maybe String
extractIPfromPeerAddress peer = matchRegex peerAddressRe peer >>= List.headMaybe

-- | True if a given peer address is externally accessable
isExternalIPv4 :: Text -> Bool
isExternalIPv4 ip = maybe False not isReserved
  where
    isReserved :: Maybe Bool
    isReserved = do
      ipAddress  <- extractIPfromPeerAddress $ Text.unpack ip
      normalized <- IPv4.decode $ Text.pack ipAddress
      return $ IPv4.reserved normalized


-- | Filter a list of peers to include only the externally accessable addresses
filterExternalPeers :: [Peer] -> [Peer]
filterExternalPeers = filter (isExternalIPv4 . peer)

-- | Get all external ipfs peer addresses
getExternalAddress :: MonadRIO          cfg m
                   => HasProcessContext cfg
                   => HasLogFunc        cfg
                   => Has IPFS.BinPath  cfg
                   => Has IPFS.Timeout  cfg
                   => m (Either Error [Peer])
getExternalAddress = IPFSProc.run' ["id"] >>= \case
    (ExitFailure _ , _, err) ->
      return $ Left $ UnknownErr $ "BOOM:" <> UTF8.textShow err

    (ExitSuccess , rawOut, _) -> do
      let
        ipfsInfo      = JSON.decode rawOut
        filteredAddrs = filterExternalPeers . maybe [] _addresses $ ipfsInfo

      logInfo $ displayShow ipfsInfo
      return $ Right filteredAddrs

fission :: Peer
fission = Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
