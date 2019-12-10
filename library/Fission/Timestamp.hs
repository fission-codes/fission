module Fission.Timestamp
  ( Unstamped
  , add
  , addM
  , withNow
  , (<@)
  ) where

import Data.Time (UTCTime)
import Fission.Prelude

type Unstamped r = UTCTime -> UTCTime -> r

add :: UTCTime -> Unstamped r -> r
add time record = record time time

addM :: MonadIO m => Unstamped r -> m r
addM record = do
  now <- liftIO getCurrentTime
  return <| add now record

(<@) :: Unstamped r -> UTCTime -> r
record <@ time = add time record

withNow :: MonadIO m => (UTCTime -> m a) -> m a
withNow cont = liftIO getCurrentTime >>= cont
