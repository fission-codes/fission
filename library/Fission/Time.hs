module Fission.Time
  ( doherty
  , dohertyMicroSeconds
  -- , getCurrentPOSIXTime
  ) where

import Fission.Prelude
-- import Data.Time.Clock.POSIX hiding (getCurrentTime)

-- | The Doherty theshold
doherty :: NominalDiffTime
doherty = 0.4

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000

-- getCurrentPOSIXTime :: MonadTime m => m Int
-- getCurrentPOSIXTime = round . utcTimeToPOSIXSeconds <$> currentTime
