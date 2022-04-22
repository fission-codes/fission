module Web.UCAN.DelegationChain.Class
  ( MonadDelegationChain(..)
  , StreamWithErrors(..)
  ) where

import           Control.Applicative
import Control.Monad
import           ListT                       (ListT)
import qualified ListT
import           RIO                         

import           Web.UCAN.Error
import           Web.UCAN.Resolver.Class


-- | A monad abstracting the effects needed for computing delegation chains.
-- Specifically:
--  - iterating over some foldable via @walk@ and
--  - short-circuiting using @attemptOrEmit@ by emitting UCAN errors
class (Alternative m, Monad m) => MonadDelegationChain m where
  attemptOrEmit :: Either Error a -> m a
  walk :: Foldable f => f a -> m a


newtype StreamWithErrors m a
  = StreamWithErrors { runStreamWithErrors :: ListT m (Either Error a) }
  deriving Functor


-- lazy instance, because ListT has the lazy instance too :(
instance Monad m => Applicative (StreamWithErrors m) where
  pure = return
  (<*>) = ap


instance Monad m => Monad (StreamWithErrors m) where
  return a = StreamWithErrors (pure (Right a))
  StreamWithErrors ma >>= f =
    StreamWithErrors do
      a <- ma
      case a of
        Right x  -> runStreamWithErrors $ f x
        Left err -> return $ Left err


instance Monad m => MonadDelegationChain (StreamWithErrors m) where
  attemptOrEmit = \case
    Right a  -> return a
    Left err -> StreamWithErrors $ return $ Left err

  walk = StreamWithErrors . fmap Right . ListT.fromFoldable


instance Monad m => Alternative (StreamWithErrors m) where
  empty = StreamWithErrors $ empty
  StreamWithErrors a <|> StreamWithErrors b = StreamWithErrors (a <|> b)


instance Resolver m => Resolver (StreamWithErrors m) where
  resolve cid = StreamWithErrors $ lift (Right <$> resolve cid)

