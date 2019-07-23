module Fission.User.CID.Lens
  ( iD
  , userFK
  , cid
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)

import Fission.User.CID.Types

makeLenses ''UserCIDT
