-- | URL helpers
module Fission.URL.Subdomain
  ( normalizePrefix
  , prefix
  , module Fission.URL.Subdomain.Types
  ) where

import Fission.Prelude
import Fission.URL.Subdomain.Types
import Fission.URL.DomainName.Types

-- | Prefix a domain named with an optional subdomain
normalizePrefix :: DomainName -> Maybe Subdomain -> DomainName
normalizePrefix domain = \case
  Nothing        -> domain
  Just subdomain -> prefix domain subdomain

-- | Prefix a domain with a subdomain
prefix :: DomainName -> Subdomain -> DomainName
prefix (DomainName domain) (Subdomain subdomain) =
  DomainName (subdomain <> "." <> domain)
