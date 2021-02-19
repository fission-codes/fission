module Fission.Process.Timeout
  ( asyncFor
  , quitAfter
  , TimedOut (..)
  )  where

import           System.Timeout

import           Fission.Prelude

import qualified Fission.Process.Status.Types as Process

data TimedOut = TimedOut
  deriving (Show, Eq, Exception)

-- FIXME Microseconds, class TimeUnit t / Duration
asyncFor :: MonadIO m => Natural -> IO a -> m (Async (Either TimedOut a))
asyncFor us io = liftIO . async $ quitAfter us io

quitAfter :: MonadIO m => Natural -> IO a -> m (Either TimedOut a)
quitAfter us io =
  liftIO do
    timeout (fromIntegral us) io >>= \case
      Nothing -> return $ Left  TimedOut
      Just a  -> return $ Right a
