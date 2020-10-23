module Fission.Web.Auth.Token.JWT.Resolver.Class (Resolves (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.JWT.RawContent     as JWT
import           Fission.Web.Auth.Token.JWT.Resolver.Error

class Monad m => Resolves m jwt where
  resolve :: CID -> m (Either Error (JWT.RawContent, jwt))
