module Fission.Process.Time
  ( asyncFor
  , quitAfter
  , sleepThread
  , module Fission.Process.Time.Error
  )  where

import           System.Timeout

-- ⚛️

import           Fission.Prelude

import           Fission.Time

import           Fission.Process.Time.Error

sleepThread :: forall m prefix s .
  ( MonadIO      m
  , Integral    (prefix s)
  , Num         (prefix Double)
  , FromPrefixed prefix Double
  )
  => Seconds prefix s
  -> m ()
sleepThread (Seconds s) = threadDelay $ truncate us
  where
    us :: Double
    Micro us = convert asDouble

    asDouble :: prefix Double
    asDouble = fromIntegral s

-- | Run an async action, with a max time
--
-- == Examples
--
-- asyncFor (Seconds (Milli 42)) (return ())
asyncFor ::
  ( MonadIO      m
  , Integral    (prefix s)
  , Num         (prefix Double)
  , FromPrefixed prefix Double
  )
  => Seconds prefix s
  -> IO a
  -> m (Async (Either TimedOut a))
asyncFor us io = liftIO . async $ quitAfter us io

-- | Quit the inner process after a period of time
--
-- == Examples
--
-- quitAfter (Seconds (Pico 1_000_000)) (return ())
quitAfter :: forall prefix s m a .
  ( MonadIO m
  , Integral    (prefix s)
  , Num         (prefix Double)
  , FromPrefixed prefix Double
  )
  => Seconds prefix s
  -> IO a
  -> m (Either TimedOut a)
quitAfter (Seconds maxTime) io =
  liftIO do
    timeout (truncate us) io >>= \case
      Nothing -> return $ Left  TimedOut
      Just a  -> return $ Right a

  where
    us :: Double
    Micro us = convert asDouble

    asDouble :: prefix Double
    asDouble = fromIntegral maxTime
