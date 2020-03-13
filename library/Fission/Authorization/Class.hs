module Fission.Authorization.Class (Authorization (..)) where

import Database.Esqueleto

import Fission.Prelude
import Fission.Models

class Authorization item where
  isOwnedBy :: UserId -> item -> Bool

instance Authorization model => Authorization (Entity model) where
  isOwnedBy userId (Entity _ model) = isOwnedBy userId model

instance Authorization (Entity User) where
  isOwnedBy userId (Entity id _) = userId == id

instance Authorization App where
  isOwnedBy userId App { appOwnerId } = appOwnerId == userId

instance Authorization Domain where
  isOwnedBy userId Domain { domainOwnerId } = userId == domainOwnerId

instance Authorization LoosePin where
  isOwnedBy userId LoosePin { loosePinOwnerId } = userId == loosePinOwnerId
