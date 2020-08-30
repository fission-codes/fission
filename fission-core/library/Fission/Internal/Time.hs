module Fission.Internal.Time
  ( roundUTC
  , toSeconds
  , fromSeconds
  ) where

import           Data.Time.Clock.POSIX

import           RIO
import           RIO.Time

roundUTC :: UTCTime -> UTCTime
roundUTC = fromSeconds . toSeconds

toSeconds :: UTCTime -> Int
toSeconds = round . utcTimeToPOSIXSeconds

fromSeconds :: Int -> UTCTime
fromSeconds = posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral
