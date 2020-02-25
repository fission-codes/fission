module Fission.URL.Subdomain.Error (NotFound (..)) where

import Fission.Prelude

import Fission.URL.DomainName.Types
import Fission.URL.Subdomain.Types

data NotFound = NotFound DomainName Subdomain
  deriving (Show, Eq)
