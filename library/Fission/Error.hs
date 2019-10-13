module Fission.Error
  ( liftE
  , withHandler
  , withHandler_
  , catchWith
  , catchWith_
  , runLogged
  ) where

import           RIO

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

-- | The same as 'withHandler', but with arguments flipped
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
-- >   BazErr -> return 3
catchWith :: Monad m => ExceptT err m a -> (err -> m a) -> m a
catchWith = flip withHandler

-- | The same as 'withHandler_', but with arguments flipped
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
catchWith_ = flip withHandler_

-- | Run inside an error-aware context, and handle all errors with a specified handler
--
-- == Examples
--
-- > withHandler (const . pure 0) do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
withHandler :: Monad m => (err -> m a) -> ExceptT err m a -> m a
withHandler errHandler actions = runExceptT actions >>= either errHandler pure

-- | Same as 'withHandler', but always returns @m ()@
--
-- == Examples
--
-- > withHandler_ (logError . displayShow) do
-- >   a <- actionA
-- >   b <- actionB
-- >   return $ a + b
withHandler_ :: Monad m => (err -> m ()) -> ExceptT err m a -> m ()
withHandler_ errHandler actions = runExceptT actions >>= either errHandler (const $ pure ())

-- | Bring an existing 'm (Either e a)' into an error-handling-aware context
--
-- == Examples
--
-- > withHandler_ (logError . displayShow) do
-- >   a <- liftE actionA
-- >   b <- liftE actionB
-- >   return $ a + b
liftE :: Functor m
      => SuperError err CLI.Error
      => m (Either err a)
      -> ExceptT CLI.Error m a
liftE = ExceptT . fmap Error.embed
