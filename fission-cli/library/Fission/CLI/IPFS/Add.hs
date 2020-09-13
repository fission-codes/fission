module Fission.CLI.IPFS.Add (dir) where

import qualified RIO.ByteString.Lazy        as Lazy

import qualified Network.IPFS               as IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error as Process

import           Fission.Prelude

dir :: IPFS.MonadLocalIPFS m => FilePath -> m (Either Process.Error CID)
dir path = IPFS.runLocal ["add", "-HQr", path] "" >>= \case
  Left  err    -> return $ Left err
  Right hashBS -> return $ Right . mkCID . decodeUtf8Lenient $ Lazy.toStrict hashBS
