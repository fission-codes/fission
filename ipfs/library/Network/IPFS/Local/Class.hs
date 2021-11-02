module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , runLocal
  ) where

import Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process

class Monad m => MonadLocalIPFS m where
  runLocal ::
       [Opt]
    -> Lazy.ByteString
    -> m (Either Process.Error Process.RawMessage)
