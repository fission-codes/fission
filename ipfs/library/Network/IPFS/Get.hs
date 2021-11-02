module Network.IPFS.Get
  ( getFile
  , getFileOrDirectory
  ) where

import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Network.IPFS.Local.Class   as IPFS
import           Network.IPFS.Prelude

import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy        as Lazy
import qualified RIO.Text                   as Text

import qualified Network.IPFS.File.Types    as File
import           Network.IPFS.Get.Error     as IPFS.Get
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Types         as IPFS

getFileOrDirectory :: MonadLocalIPFS m => IPFS.CID -> m (Either IPFS.Get.Error CL.ByteString)
getFileOrDirectory cid@(IPFS.CID hash) = IPFS.runLocal ["get", Text.unpack hash] "" >>= \case
  Right contents -> return $ Right contents
  Left err -> case err of
    Process.Timeout secs   -> return . Left $ TimedOut cid secs
    Process.UnknownErr raw -> return . Left . UnknownErr $ UTF8.textShow raw

getFile :: MonadLocalIPFS m => IPFS.CID -> m (Either IPFS.Get.Error File.Serialized)
getFile cid@(IPFS.CID hash) = IPFS.runLocal ["cat"] (UTF8.textToLazyBS hash) >>= \case
  Right contents -> return . Right $ File.Serialized contents
  Left err -> case err of
    Process.Timeout secs -> return . Left $ TimedOut cid secs
    Process.UnknownErr raw ->
      if Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" raw
        then return . Left $ InvalidCID hash
        else return . Left . UnknownErr $ UTF8.textShow raw
