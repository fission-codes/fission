module Fission.Error
  ( liftE
  , withHandler
  , withHandler_
  , catch
  , catch_
  , runLogged
  ) where

import           RIO hiding (catch)

import           Control.Monad.Except

import           Fission.Error.Class     as Error
import qualified Fission.CLI.Error.Types as CLI
import           Fission.Internal.Constraint

-- | Run inside a context that understands automated error logging.
--   Returns the result of the underlying action, and a simple 'Either'
runLogged :: MonadRIO   cfg m
          => HasLogFunc cfg
          => Show err
          => ExceptT err m ()
          -> m ()
runLogged actions = withHandler (logError . displayShow) actions

-- | Run inside an error-aware context, and handle all errors with a specified handler
-- withHandler :: Monad m => (Either e a -> m c) -> ExceptT e m a -> m c
-- withHandler errHandler actions = errHandler =<< runExceptT actions

-- withHandler :: Monad m
--             => SuperError subErr supErr
--             => Show supErr
--             => (supErr -> m a)
--             -> ExceptT subErr m a
--             -> m a
-- withHandler = withHandler'

catch :: Monad m
      -- => SuperError subErr supErr
      => ExceptT err m a
      -> (err -> m a)
      -> m a
catch = flip withHandler

catch_ :: Monad m
       -- => SuperError subErr supErr
       => ExceptT err m a
       -> (err -> m ())
       -> m ()
catch_ = flip withHandler_

withHandler :: Monad m
            => (err -> m a)
            -> ExceptT err m a
            -> m a
withHandler errHandler actions = runExceptT actions >>= either errHandler pure

withHandler_ :: Monad m
             => (err -> m ())
             -> ExceptT err m a
             -> m ()
withHandler_ errHandler actions = runExceptT actions >>= either errHandler (const $ pure ())

-- | Bring an existing 'm (Either e a)' into an error-handling-aware context
liftE :: Functor m
      => SuperError err CLI.Error
      => m (Either err a)
      -> ExceptT CLI.Error m a
liftE = ExceptT . fmap Error.embed
