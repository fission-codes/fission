{-# LANGUAGE ImpredicativeTypes #-}
module Fission.Platform.Heroku.AddOn.Lens
  ( iD
  , uuid
  , region
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens                        (makeLenses)
-- import Database.Beam
import Fission.Platform.Heroku.AddOn.Types

makeLenses ''AddOnT

-- AddOn
--   (LensFor iD        )
--   (LensFor uuid      )
--   (LensFor region    )
--   (LensFor insertedAt)
--   (LensFor modifiedAt) = tableLenses
