module Fission.User.Lens
  ( userID
  , role
  , active
  , herokuAddOnId
  , secretDigest
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)

import Fission.User.Types

makeLenses ''User
