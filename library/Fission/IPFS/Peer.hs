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

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8

import qualified Fission.IPFS.Process        as IPFSProc
import qualified Fission.IPFS.Types          as IPFS
import           Fission.IPFS.Peer.Error     as IPFS.Peer
import           Fission.IPFS.Peer.Types
import           Fission.IPFS.Info.Types
import qualified Data.Aeson as JSON
import           Fission.IPFS.Peer.Types

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

isIP4 :: Text -> Bool
isIP4 = Text.isInfixOf "ip4"

isLocal :: Text -> Bool
isLocal addr = Text.isInfixOf "127." addr || Text.isInfixOf "localhost" addr -- TODO: Handle localhost

isExternalIP4 :: Text -> Bool
isExternalIP4 addr = (not $ isLocal addr) && (isIP4 addr)

getExternalAddress :: (MonadIO m
  , MonadReader cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg)
  => m (Either Error [Peer])
getExternalAddress = IPFSProc.run ["id"] "" >>= pure . \case
  (ExitFailure _ , _, err) ->
    Left $ UnknownErr $ UTF8.textShow err

  (ExitSuccess , rawOut, _) -> do
    -- TODO break into a get ipfs info function, a get address function then a filter function
    let ipfsInfo = JSON.decode rawOut
    let filteredAddrs = (filter isExternalIP4) . maybe [""] _addresses $ ipfsInfo
    Right $ fmap Peer filteredAddrs

fission :: Peer
fission = Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
