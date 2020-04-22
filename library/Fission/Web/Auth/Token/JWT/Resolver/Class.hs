module Fission.Web.Auth.Token.JWT.Resolver.Class (Resolver (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
 
import           Fission.Web.Auth.Token.JWT
import           Fission.Web.Auth.Token.JWT.Resolver.Error

class Monad m => Resolver m where
  resolve :: CID -> m (Either Error (Text, JWT))
