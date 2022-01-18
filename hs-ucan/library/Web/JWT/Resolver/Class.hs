module Web.JWT.Resolver.Class (Resolver (..)) where

import           Network.IPFS.CID.Types

import           RIO

import           Web.JWT.Resolver.Error as Resolver
import           Web.JWT.Types

import qualified Web.JWT.RawContent     as JWT

class Monad m => Resolver m fct rsc where
  resolve :: CID -> m (Either Resolver.Error (JWT.RawContent, JWT fct rsc))
