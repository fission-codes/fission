module Fission.Process.Time
  ( asyncFor
  , quitAfter
  , sleepThread
  , module Fission.Process.Time.Error
  )  where

import qualified Data.Fixed                 as Fixed
import           System.Timeout

import           Fission.Prelude

import           Fission.Process.Time.Error

sleepThread ::
  ( MonadIO      m
  , Functor      prefix
  , FromPrefixed prefix
  )
  => prefix (Seconds Natural)
  -> m ()
sleepThread s = threadDelay . truncate . getSeconds $ getMicro usInt
  where
    usInt :: Micro (Seconds (Fixed.Fixed 1))
    usInt = changePrefix $ asFixed s

-- | Run an async action, with a max time
--
-- == Examples
--
-- asyncFor (Milli (Seconds 42)) (return ())
asyncFor ::
  ( MonadIO      m
  , Functor      prefix
  , FromPrefixed prefix
  )
  => prefix (Seconds Natural)
  -> IO a
   -> m (Async (Either TimedOut a))
asyncFor us io = liftIO . async $ quitAfter us io

-- | Quit the inner process after a period of time
--
-- == Examples
--
-- quitAfter (Pico (Seconds 1_000_000)) (return ())
quitAfter ::
  ( MonadIO      m
  , Functor      prefix
  , FromPrefixed prefix
  )
  => prefix (Seconds Natural)
  -> IO a
  -> m (Either TimedOut a)
quitAfter maxTime io =
  liftIO do
    timeout (fromIntegral us) io >>= \case
      Nothing -> return $ Left  TimedOut
      Just a  -> return $ Right a

  where
    Micro (Seconds us) = asNatural fixed

    fixed :: Micro (Seconds (Fixed.Fixed 0))
    fixed = changePrefix $ mapScalar fromIntegral maxTime
