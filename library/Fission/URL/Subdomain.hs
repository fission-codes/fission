-- | URL helpers
module Fission.URL.Subdomain
  ( subdomain
  , prefix
  ) where

import Fission.Prelude
import Fission.URL.Subdomain.Types

-- | Prefix a domain named with an optional subdomain
normalize :: Text -> Maybe Subdomain -> Text
normalize domain = \case
  Nothing        -> domain
  Just subdomain -> prefix subdomain domain

-- | Prefix a domain with a subdomain
prefix :: Text -> Text -> Text
prefix rawSubdomain domain = rawSubdomain <> "." <> domain
