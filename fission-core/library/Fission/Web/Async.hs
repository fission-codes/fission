-- | The names in this module seem *wild*, but they're extensions of 'Control.Concurrent.Async' that know about our use case
module Fission.Web.Async
  ( waitAnySuccessCatch
  , waitAnySuccessCatchCancel
  , waitAll
  , asyncIdleTimeout
  ) where

-- 🧱

import qualified RIO.List             as List
import           RIO.NonEmpty         as NonEmpty

import qualified Control.Exception    as Exception

-- 🌐

import           Servant.Client

-- ⚛️

import           Fission.Prelude

import qualified Fission.Process.Time as Process
import           Fission.Time

-- | Wait for the first to finish successfully, or for all to fail.
--   Remaining processes continue without being quit.
waitAnySuccessCatch ::
  MonadIO m
  => NonEmpty (Async (Either ClientError a))
  -> m (Either ClientError ([Async (Either ClientError a)], a))
waitAnySuccessCatch asyncRefs = do
  let listAsyncs = NonEmpty.toList asyncRefs
  (finishedAsync, result) <- liftIO $ waitAnyCatch listAsyncs

  let remainingList = List.delete finishedAsync listAsyncs

  case normalizeResult result of
    Left err ->
      case nonEmpty remainingList of
        Nothing        -> return $ Left err
        Just remaining -> waitAnySuccessCatch remaining

    Right val ->
      return $ Right (remainingList, val)

asyncIdleTimeout ::
  ( MonadIO m
  , Eq      a
  , Integral (prefix n)
  , Num      (prefix Double)
  , FromPrefixed prefix Double
  )
  => Seconds prefix n
  -> Async a
  -> TVar  (Maybe a)
  -> m (Async (Either Process.TimedOut ()))
asyncIdleTimeout idleTimeout asyncRef latestVar =
  liftIO . async $ go Nothing
  where
    go lastKnown = do
      Process.sleepThread idleTimeout
      latest <- readTVarIO latestVar
      if latest == lastKnown
        then do
          asyncRef `cancelWith` Process.TimedOut
          return $ Left Process.TimedOut

        else
          go latest

-- | First past the post / classic race strategy. Cancel all when
waitAnySuccessCatchCancel ::
  MonadIO m
  => NonEmpty (Async (Either ClientError a))
  -> m (Either ClientError a)
waitAnySuccessCatchCancel asyncRefs = do
  result <- liftIO $ waitAnySuccessCatch asyncRefs `Exception.finally` mapM_ cancel asyncRefs
  case result of
    Left err     -> return $ Left err
    Right (_, a) -> return $ Right a

-- | Wait for all cluster peers to complete.
waitAll :: MonadIO m => NonEmpty (Async (Either ClientError a)) -> m (NonEmpty (Either ClientError a))
waitAll asyncRefs =
  liftIO $ forConcurrently asyncRefs \ref ->
    normalizeResult <$> waitCatch ref

normalizeResult :: Either SomeException (Either ClientError a) -> Either ClientError a
normalizeResult = \case
  Left someException       -> Left $ ConnectionError someException
  Right (Left clientError) -> Left clientError
  Right (Right val)        -> Right val
