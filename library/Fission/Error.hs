module Fission.Error
  ( liftE
  , log
  , runLogged
  , withHandler
  , handleable
  ) where

import           RIO hiding (log)

import           Control.Monad.Except

import           Fission.Error.Class     as Error
import qualified Fission.CLI.Error.Types as CLI
import           Fission.Internal.Constraint

-- | Run inside a context that understands automated error logging.
--   Returns the result of the underlying action, and a simple 'Either'
runLogged :: MonadRIO   cfg m
          => HasLogFunc cfg
          => Show    err
          => ExceptT err m a
          -> m (Either err a)
-- maybe runExitLogger
runLogged actions = withHandler log actions

-- | Run inside an error-aware context, and handle all errors with a specified handler
withHandler :: Monad m => (Either e a -> m c) -> ExceptT e m a -> m c
withHandler errHandler actions = errHandler =<< runExceptT actions

-- | Log an error (if any currently), and continue
log :: (MonadRIO cfg m, HasLogFunc cfg, Show err) => Either err a -> m (Either err a)
log (Right val) = pure $ Right val
log (Left  err) = do
  logDebug $ displayShow err
  return $ Left err

-- | Bring an existing 'm (Either e a)' into an error-handling-aware context
liftE :: Functor m
      => SuperError err CLI.Error
      => m (Either err a)
      -> ExceptT CLI.Error m a
liftE = ExceptT . fmap Error.embed

handleable :: Functor m
           => SuperError err CLI.Error
           => ExceptT err       m a
           -> ExceptT CLI.Error m a
handleable = withExceptT Error.toError
