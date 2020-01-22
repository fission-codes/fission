module Fission.Internal.Fixture.Time (agesAgo) where

import           Data.Time as Time

agesAgo :: UTCTime
agesAgo = UTCTime
  { utctDay     = ModifiedJulianDay { toModifiedJulianDay = 0 }
  , utctDayTime = Time.secondsToDiffTime 0
  }
