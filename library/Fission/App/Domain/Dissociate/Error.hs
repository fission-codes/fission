module Fission.App.Domain.Dissociate.Error (NotRegisteredToApp (..)) where

import Fission.Prelude
import Fission.Models
import Fission.URL

data NotRegisteredToApp
  = NotRegisteredToApp AppId DomainName (Maybe Subdomain)
  deriving (Show, Eq)
