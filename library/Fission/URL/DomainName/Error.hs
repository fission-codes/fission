module Fission.URL.DomainName.Error (NotFound (..)) where

import Fission.Prelude

import Fission.URL.DomainName.Types

data NotFound = NotFound DomainName
  deriving (Show, Eq)
