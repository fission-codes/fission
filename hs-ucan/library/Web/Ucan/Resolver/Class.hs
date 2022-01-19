module Web.Ucan.Resolver.Class (Resolver (..)) where

import           Network.IPFS.CID.Types

import           RIO

import           Web.Ucan.Resolver.Error as Resolver
import           Web.Ucan.Types

import qualified Web.Ucan.RawContent     as JWT

class Monad m => Resolver m fct rsc where
  resolve :: CID -> m (Either Resolver.Error (JWT.RawContent, JWT fct rsc))
