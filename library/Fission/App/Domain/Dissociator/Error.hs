module Fission.App.Domain.Dissociator.Error (NotRegisteredToApp (..)) where

import Fission.Prelude
import Fission.Models
import Fission.URL

data NotRegisteredToApp
  = NotRegisteredToApp AppId DomainName (Maybe Subdomain)
  deriving (Show, Eq)
