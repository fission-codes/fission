module Fission.IPFS.Peer
  ( all
  , rawList
  ) where

import           RIO hiding (all)
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text
import           RIO.Process (HasProcessContext)

import SuperRecord

import           Fission.Internal.Constraint
import qualified Fission.IPFS.Process        as IPFSProc
import qualified Fission.IPFS.Types          as IPFS
import           Fission.IPFS.Peer.Error     as IPFS.Peer
import qualified Fission.Internal.UTF8       as UTF8

all :: MonadRIO          (Rec cfg) m
    => HasProcessContext (Rec cfg)
    => HasLogFunc        (Rec cfg)
    => Has "ipfsPath"         cfg IPFS.BinPath
    => Has "ipfsTimeout"      cfg IPFS.Timeout
    => m (Either IPFS.Peer.Error [IPFS.Peer])
all = rawList <&> \case
  (ExitSuccess, allRaw, _) ->
    case UTF8.encode allRaw of
      Left  _    -> Left . DecodeFailure $ show allRaw
      Right text -> Right $ IPFS.Peer <$> Text.lines text

  (ExitFailure _, _, err) ->
    Left . UnknownErr $ UTF8.textShow err

rawList :: MonadRIO          (Rec cfg) m
        => Has "ipfsPath"         cfg IPFS.BinPath
        => Has "ipfsTimeout"      cfg IPFS.Timeout
        => HasProcessContext (Rec cfg)
        => HasLogFunc        (Rec cfg)
        => m (ExitCode, Lazy.ByteString, Lazy.ByteString)
rawList = IPFSProc.run' ["bootstrap", "list"]
