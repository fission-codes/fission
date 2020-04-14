module Fission.Web.Auth.JWT.Proof.Validation.Error (Error (..)) where

import Fission.Prelude

import qualified Fission.Web.Auth.JWT.Proof.Resolver.Class as Resolver

data Error
  = InvalidSignatureChain
  | ScopeOutOfBounds
  | PotencyEscelation
  | ResolverIssue Resolver.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    InvalidSignatureChain -> "Invalid signature chain"
    ScopeOutOfBounds      -> "Path scope not in delegated rights"
    PotencyEscelation     -> "Potency escelation"
    ResolverIssue resErr  -> "Unable to resolve CID proof: " <> display resErr
