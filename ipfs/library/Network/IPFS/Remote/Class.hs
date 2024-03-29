module Network.IPFS.Remote.Class
  ( MonadRemoteIPFS
  , MfsCopyArgs (..)
  , MfsStatArgs (..)
  , MfsRemoveArgs (..)
  , MfsWriteArgs (..)
  , runRemote
  , ipfsAdd
  , ipfsCat
  , ipfsStat
  , ipfsPin
  , ipfsUnpin
  , mfsCopy
  , mfsRemove
  , mfsStat
  , mfsWrite
  ) where

import           Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy                        as Lazy
import           Servant.Client

import           Network.IPFS.Types                         as IPFS

import qualified Network.IPFS.Client                        as IPFS.Client
import qualified Network.IPFS.Client.Pin                    as Pin
import qualified Network.IPFS.File.Types                    as File
import qualified Network.IPFS.Client.Files.Statistics.Types as Files.Statistics
import qualified Network.IPFS.Client.Files.Write.Form.Types as Files.Write


data MfsCopyArgs = MfsCopyArgs { from :: Text, to :: Text, parents :: Bool }
data MfsRemoveArgs = MfsRemoveArgs { path :: Text, recursive :: Bool, force :: Maybe Bool }
data MfsStatArgs = MfsStatArgs { path :: Text }
data MfsWriteArgs = MfsWriteArgs
                      { path :: Text
                      , create :: Bool
                      , parents :: Bool
                      , truncate :: Bool
                      , rawLeaves :: Maybe Bool
                      , cidVersion :: Maybe Integer
                      , hash :: Maybe Text
                      }


class MonadIO m => MonadRemoteIPFS m where
  runRemote :: ClientM a       -> m (Either ClientError a)
  ipfsAdd   :: Lazy.ByteString -> m (Either ClientError CID)
  ipfsCat   :: CID             -> m (Either ClientError File.Serialized)
  ipfsStat  :: CID             -> m (Either ClientError Stat)
  ipfsPin   :: CID             -> m (Either ClientError Pin.Response)
  ipfsUnpin :: CID -> Bool     -> m (Either ClientError Pin.Response)

  mfsCopy     :: MfsCopyArgs                                          -> m (Either ClientError ())
  mfsRemove   :: MfsRemoveArgs                                        -> m (Either ClientError ())
  mfsStat     :: MfsStatArgs                                          -> m (Either ClientError Files.Statistics.Response)
  mfsWrite    :: MfsWriteArgs -> (Lazy.ByteString, Files.Write.Form)  -> m (Either ClientError ())

  -- defaults
  ipfsAdd   raw           = runRemote $ IPFS.Client.add   raw
  ipfsCat   cid           = runRemote $ IPFS.Client.cat   cid
  ipfsPin   cid           = runRemote $ IPFS.Client.pin   cid
  ipfsUnpin cid recursive = runRemote $ IPFS.Client.unpin cid recursive
  ipfsStat  cid           = runRemote $ IPFS.Client.stat  cid

  mfsCopy (MfsCopyArgs { .. })        = runRemote $ IPFS.Client.filesCopy from to parents
  mfsRemove (MfsRemoveArgs { .. })    = runRemote $ IPFS.Client.filesRemove path recursive force
  mfsStat (MfsStatArgs { .. })        = runRemote $ IPFS.Client.filesStat path
  mfsWrite (MfsWriteArgs { .. }) raw  = runRemote $ IPFS.Client.filesWrite path create parents truncate rawLeaves cidVersion hash raw
