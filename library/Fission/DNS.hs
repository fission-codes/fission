-- | URL helpers
module Fission.URL
  ( subdomain
  , prefix
  ) where

import Fission.Prelude

-- | Prefix a domain named with an optional subdomain
subdomain :: IsString str -> str -> Maybe str -> str
subdomain domain = \case
  Nothing        -> domain
  Just subdomain -> prefix subdomain domain

-- | Prefix a domain with a subdomain
prefix :: IsString str => str -> str -> str
prefix subdomain domain = subdomain <> "." <> domain
