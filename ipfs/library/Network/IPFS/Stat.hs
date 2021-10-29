module Network.IPFS.Stat
  ( getStatRemote
  , getSizeRemote
  , getSize
  , module Network.IPFS.Stat.Types
  ) where

import           Data.ByteString.Lazy.Char8 as CL

import qualified RIO.ByteString.Lazy        as Lazy
import qualified RIO.List                   as List

import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Network.IPFS.Local.Class   as IPFS
import           Network.IPFS.Prelude
import           Network.IPFS.Remote.Class  as Remote

import           Network.IPFS.Get.Error     as IPFS.Get
import qualified Network.IPFS.Process.Error as Process

import           Network.IPFS.Bytes.Types
import           Network.IPFS.Stat.Types
import           Network.IPFS.Types         as IPFS

getStatRemote :: MonadRemoteIPFS m => IPFS.CID -> m (Either IPFS.Get.Error Stat)
getStatRemote cid =
  Remote.ipfsStat cid >>= \case
    Right statPayload -> return $ Right statPayload
    Left err          -> return . Left $ IPFS.Get.WebError err

getSizeRemote :: MonadRemoteIPFS m => IPFS.CID -> m (Either IPFS.Get.Error Bytes)
getSizeRemote cid =
  getStatRemote cid >>= \case
    Left err ->
      return $ Left err

    Right Stat {cumulativeSize} ->
      case cumulativeSize of
        Left err   -> return $ Left $ IPFS.Get.SizeError err
        Right size -> return $ Right size

getSize :: MonadLocalIPFS m => IPFS.CID -> m (Either IPFS.Get.Error Integer)
getSize cid@(CID hash) = IPFS.runLocal ["object", "stat"] (Lazy.fromStrict <| encodeUtf8 hash) >>= \case
  Left err -> case err of
    Process.Timeout secs   -> return . Left $ TimedOut cid secs
    Process.UnknownErr raw -> return . Left . UnknownErr $ UTF8.textShow raw

  Right contents ->
    case parseSize contents of
      Nothing        -> return . Left . UnexpectedOutput $ "Could not parse CumulativeSize"
      Just (size, _) -> return $ Right size

parseSize :: Lazy.ByteString -> Maybe (Integer, Lazy.ByteString)
parseSize lbs = do
  finalLine <- List.lastMaybe $ CL.lines lbs
  finalWord <- List.lastMaybe $ CL.words finalLine
  CL.readInteger finalWord
