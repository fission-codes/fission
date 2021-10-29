module Network.IPFS.Peer
  ( all
  , rawList
  , connect
  , connectRetry
  , disconnect
  , getExternalAddress
  ) where

import qualified RIO.List                   as List
import qualified RIO.Text                   as Text

import qualified Network.IP.Addr            as Addr

import           Text.Regex

import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Network.IPFS.Prelude       hiding (all)

import           Network.IPFS.Info.Types
import           Network.IPFS.Local.Class   as IPFS
import           Network.IPFS.Peer.Error    as IPFS.Peer
import           Network.IPFS.Peer.Types
import qualified Network.IPFS.Process.Error as Process
import qualified Network.IPFS.Types         as IPFS

all :: MonadLocalIPFS m => m (Either IPFS.Peer.Error [IPFS.Peer])
all = rawList <&> \case
  Right raw -> case UTF8.encode raw of
    Left  _    -> Left  . DecodeFailure $ show raw
    Right text -> Right $ IPFS.Peer <$> Text.lines text
  Left err -> Left . UnknownErr $ UTF8.textShow err

rawList :: MonadLocalIPFS m => m (Either Process.Error Process.RawMessage)
rawList = IPFS.runLocal ["bootstrap", "list"] ""

connect :: MonadLocalIPFS m => Peer -> m (Either IPFS.Peer.Error ())
connect peer@(Peer peerID) = IPFS.runLocal ["swarm", "connect"] (UTF8.textToLazyBS peerID) >>= pure . \case
  Left _  -> Left $ CannotConnect peer
  Right _ -> Right ()

disconnect :: MonadLocalIPFS m => Peer -> m (Either IPFS.Peer.Error ())
disconnect peer@(Peer peerID) =
  IPFS.runLocal ["swarm", "disconnect"] (UTF8.textToLazyBS peerID) >>= pure . \case
    Left _  -> Left $ CannotDisconnect peer
    Right _ -> Right ()

connectRetry :: MonadLocalIPFS m => Peer -> Natural -> m (Either IPFS.Peer.Error ())
connectRetry peer 0 = return . Left $ CannotConnect peer
connectRetry peer tries = connect peer >>= \case
  Right _   -> return $ Right ()
  Left _err -> connectRetry peer (tries - 1)

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
      normalized <- readMaybe ipAddress
      return (Addr.ip4Range normalized == Addr.ReservedIP4)

-- | Filter a list of peers to include only the externally accessable addresses
filterExternalPeers :: [Peer] -> [Peer]
filterExternalPeers = filter (isExternalIPv4 . peer)

-- | Get all external ipfs peer addresses
getExternalAddress :: MonadLocalIPFS m => m (Either IPFS.Peer.Error [Peer])
getExternalAddress =
  IPFS.runLocal ["id"] "" >>= \case
    Left err ->
      return . Left . UnknownErr $ UTF8.textShow err

    Right raw ->
      raw
        |> decode
        |> maybe [] addresses
        |> Right . filterExternalPeers
        |> pure
