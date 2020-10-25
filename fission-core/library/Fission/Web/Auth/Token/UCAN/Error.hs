module Fission.Web.Auth.Token.UCAN.Error (Error (..)) where

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as JWT.Resolver

data Error
  = StructuralError JWT.Error
  | ResolverError   JWT.Resolver.Error
  | UserNotFound
  | NoProof
  | ValidationError -- FIXME more detail
  deriving (Show, Eq)
