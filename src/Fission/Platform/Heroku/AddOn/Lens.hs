module Fission.Platform.Heroku.AddOn.Lens
  ( addOnID
  , uuid
  , region
  , insertedAt
  , modifiedAt
  ) where

import Control.Lens (makeLenses)
import Fission.Platform.Heroku.AddOn.Types

makeLenses ''AddOn
