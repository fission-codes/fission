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
import           RIO.Process (HasProcessContext)

import           Data.Has
import Net.IPv4
import Text.Regex
import Data.List hiding (all)
import qualified Data.Text as Text
import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFSProc
import qualified Fission.IPFS.Types          as IPFS
import           Fission.IPFS.Peer.Error     as IPFS.Peer
import           Fission.IPFS.Peer.Types
import           Fission.IPFS.Info.Types
import qualified Data.Aeson as JSON
-- import           Fission.IPFS.Peer.Types

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

peerAddressRe = mkRegex "^/ip[46]/([a-zA-Z0-9.:]*)/"

extractIPfromPeerAddress peer = peer
  & matchRegex peerAddressRe
  <&> Data.List.head

isExternalIPv4 :: Text -> Bool
isExternalIPv4 ip = ip
  & Text.unpack
  & extractIPfromPeerAddress
  & maybe "" Text.pack
  & Net.IPv4.decode
  <&> Net.IPv4.reserved
  & maybe False not


filterExternalPeers :: [Peer] -> [Peer]
filterExternalPeers = Data.List.filter (isExternalIPv4 . peer)

getExternalAddress :: (MonadIO m
  , MonadReader cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg)
  => m (Either Error [Peer])
getExternalAddress = IPFSProc.run' ["id"] >>= \case
    (ExitFailure _ , _, err) ->
      return $ Left $ UnknownErr $ "BOOM:" <> UTF8.textShow err

    (ExitSuccess , rawOut, _) -> do
      let ipfsInfo = JSON.decode rawOut
      let filteredAddrs = filterExternalPeers . maybe [] _addresses $ ipfsInfo

      logInfo $ displayShow ipfsInfo

      return $ Right filteredAddrs

fission :: Peer
fission = Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
