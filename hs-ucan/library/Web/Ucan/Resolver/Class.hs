module Web.Ucan.Resolver.Class (Resolver (..)) where

import           Network.IPFS.CID.Types

import           RIO

import           Web.Ucan.Resolver.Error as Resolver
import           Web.Ucan.Types

import qualified Web.Ucan.RawContent     as Ucan

class Monad m => Resolver m fct rsc where
  resolve :: CID -> m (Either Resolver.Error (Ucan.RawContent, Ucan fct rsc))
