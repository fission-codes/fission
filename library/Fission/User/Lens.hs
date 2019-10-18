module Fission.User.Lens
  ( userID
  , username
  , email
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
