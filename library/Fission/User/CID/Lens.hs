module Fission.User.CID.Lens
  ( userCID
  , userFK
  , cid
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)

import Fission.User.CID.Types

makeLenses ''UserCID
