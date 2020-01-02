module Fission.URL.Subdomain.Types (Subdomain (..)) where

import Fission.Prelude

newtype Subdomain = Subdomain { getSubdomain :: Text }
