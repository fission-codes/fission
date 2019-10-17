module Fission.User.Lens
  ( userID
  , role
  , active
  , herokuAddOnID
  , secretDigest
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)

import Fission.User.Types

makeLenses ''User
