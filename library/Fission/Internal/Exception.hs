module Fission.Internal.Exception
  ( liftE
  , handleWith
  , handleWith_
  , catchWith
  , catchWith_
  , runLogged
  ) where

import           RIO

import           Control.Monad.Except

import qualified Data.Bifunctor as BF

-- import           Fission.Error.Class     as Error
-- import qualified Fission.CLI.Error.Types as CLI
import           Fission.Internal.Constraint

-- | Run inside a context that understands automated error logging.
--   Returns the result of the underlying action, and a simple 'Either'
runLogged :: MonadRIO   cfg m
          => HasLogFunc cfg
          => Show err
          => ExceptT err m ()
          -> m ()
runLogged actions = handleWith (logError . displayShow) actions

-- | The same as 'handleWith', but with arguments flipped
--
-- == Examples
--
-- > do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
-- > `catchWith` \case
-- >   FooErr -> return 1
-- >   BarErr -> return 2
-- >   BazErr -> return 3catchWith :: Monad m => ExceptT err m a -> (err -> m a) -> m a

catchWith :: Monad m => ExceptT err m a -> (err -> m a) -> m a
catchWith = flip handleWith

-- | The same as 'handleWith_', but with arguments flipped
--
-- == Examples
--
-- > do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
-- > `catchWith` \case
-- >   FooErr -> logError "Didn't work"
-- >   BarErr -> logError "Really didn't work"
-- >   BazErr -> logError "Really really eally didn't work"
catchWith_ :: Monad m => ExceptT err m a -> (err -> m ()) -> m ()
catchWith_ = flip handleWith_

-- | Run inside an error-aware context, and handle all errors with a specified handler
--
-- == Examples
--
-- > handleWith (const . pure 0) do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
handleWith :: Monad m => (err -> m a) -> ExceptT err m a -> m a
handleWith errHandler actions = runExceptT actions >>= either errHandler pure

-- | Same as 'handleWith', but always returns @m ()@
--
-- == Examples
--
-- > handleWith_ (logError . displayShow) do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
handleWith_ :: Monad m => (err -> m ()) -> ExceptT err m a -> m ()
handleWith_ errHandler actions = runExceptT actions >>= either errHandler (const $ pure ())

-- | Bring an existing 'm (Either e a)' into an error-handling-aware context
--
-- == Examples
--
-- > handleWith_ (logError . displayShow) do
-- >   a <- liftE actionA
-- >   b <- liftE actionB
-- >   return $ a + b
liftE :: (Functor m, Exception e) => m (Either e a) -> ExceptT SomeException m a
liftE = ExceptT . fmap (BF.first toException)
