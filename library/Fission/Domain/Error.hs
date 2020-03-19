module Fission.Domain.Error (DoesNotExist (..)) where

import Fission.Prelude
import Fission.URL.DomainName.Types

data DoesNotExist = DoesNotExist DomainName
  deriving (Show, Eq)
