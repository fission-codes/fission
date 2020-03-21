-- | Validate user

module Fission.User.Validation
  ( check
  , isValid
  ) where

import           Fission.Prelude
import           Fission.Models
 
import qualified Fission.User.Username as Username

check :: User -> Either Username.Invalid User
check user =
  if isValid user
    then Right user
    else Left Username.Invalid

isValid :: User -> Bool
isValid (User {userUsername}) = Username.isValid userUsername
