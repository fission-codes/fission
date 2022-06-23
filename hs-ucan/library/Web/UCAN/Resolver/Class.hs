module Web.UCAN.Resolver.Class (Resolver (..)) where

import           Network.IPFS.CID.Types

import           RIO

import           Web.UCAN.Resolver.Error as Resolver

class Monad m => Resolver m where
  resolve :: CID -> m (Either Resolver.Error ByteString)
