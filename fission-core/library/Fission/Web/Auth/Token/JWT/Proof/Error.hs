module Fission.Web.Auth.Token.JWT.Proof.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Resolver as JWT.Resolver

data Error
  = InvalidSignatureChain
  | ResourceEscelation
  | CapabilityEscelation
  | TimeNotSubset
  | ResolverError JWT.Resolver.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    InvalidSignatureChain -> "Invalid signature chain"
    ResourceEscelation    -> "Resource escelation"
    CapabilityEscelation  -> "Capabilty escelation"
    TimeNotSubset         -> "Time bounds are not a subset"
    ResolverError resErr  -> "Unable to resolve CID proof: " <> display resErr

instance ToServerError Error where
  toServerError = \case
    InvalidSignatureChain -> err422 { errBody = displayLazyBS InvalidSignatureChain }
    ResourceEscelation    -> err401 { errBody = displayLazyBS ResourceEscelation    }
    CapabilityEscelation  -> err401 { errBody = displayLazyBS CapabilityEscelation  }
    TimeNotSubset         -> err422 { errBody = displayLazyBS TimeNotSubset         }
    ResolverError err     -> toServerError err
