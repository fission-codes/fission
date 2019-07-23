{-# LANGUAGE ImpredicativeTypes #-}
module Fission.User.Lens
  ( iD
  , role
  , active
  , herokuAddOnId
  , secretDigest
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)

import           Database.Beam
import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn
import           Fission.User.Types

makeLenses ''UserT

-- User
--   (LensFor iD)
--   (LensFor role)
--   (LensFor active )
--   (Heroku.AddOn.ID (LensFor herokuAddOnId))
--   (LensFor secretDigest)
--   (LensFor insertedAt)
--   (LensFor modifiedAt) = tableLenses
