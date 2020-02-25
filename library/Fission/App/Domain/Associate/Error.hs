module Fission.App.Domain.Associate.Error (AlreadyExists (..)) where

import Fission.Prelude
import Fission.Models
import Fission.URL

data AlreadyExists
  = AlreadyExists AppId DomainName (Maybe Subdomain)
  deriving (Show, Eq)
