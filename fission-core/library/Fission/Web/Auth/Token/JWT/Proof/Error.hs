module Fission.Web.Auth.Token.JWT.Proof.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Resolver as JWT.Resolver

data Error
  = InvalidSignatureChain
  | ScopeOutOfBounds -- FIXME remove?
  | ResourceEscelation
  | CapabilityEscelation
  | TimeNotSubset
  | ResolverError JWT.Resolver.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    InvalidSignatureChain -> "Invalid signature chain"
    ScopeOutOfBounds      -> "Path scope not in delegated rights"
    ResourceEscelation    -> "Resource escelation"
    CapabilityEscelation  -> "Capabilty escelation"
    TimeNotSubset         -> "Time bounds are not a subset"
    ResolverError resErr  -> "Unable to resolve CID proof: " <> display resErr

instance ToServerError Error where
  toServerError = \case
    ResolverError err     -> toServerError err
    ScopeOutOfBounds      -> err401 { errBody = displayLazyBS ScopeOutOfBounds      }
    CapabilityEscelation  -> err401 { errBody = displayLazyBS CapabilityEscelation  }
    TimeNotSubset         -> err422 { errBody = displayLazyBS TimeNotSubset         }
    InvalidSignatureChain -> err422 { errBody = displayLazyBS InvalidSignatureChain }
