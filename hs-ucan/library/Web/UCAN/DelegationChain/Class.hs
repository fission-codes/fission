module Web.UCAN.DelegationChain.Class
  ( MonadWalk(..)
  , StreamWithErrors(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           ListT                   (ListT)
import qualified ListT
import           RIO

import           Control.Monad.Except
import           Web.UCAN.Error
import           Web.UCAN.Resolver.Class


-- | A monad abstracting over iterating elements
class Alternative m => MonadWalk m where
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
    StreamWithErrors $
      ma >>= either (return . Left) (runStreamWithErrors . f)


instance Monad m => MonadWalk (StreamWithErrors m) where
  walk = StreamWithErrors . fmap Right . ListT.fromFoldable


instance Monad m => Alternative (StreamWithErrors m) where
  empty = StreamWithErrors $ empty
  StreamWithErrors a <|> StreamWithErrors b = StreamWithErrors (a <|> b)


instance Resolver m => Resolver (StreamWithErrors m) where
  resolve cid = StreamWithErrors $ lift (Right <$> resolve cid)


instance Monad m => MonadError Error (StreamWithErrors m) where
  throwError = StreamWithErrors . return . Left
  catchError (StreamWithErrors listt) handler =
    StreamWithErrors $ do
      listt >>= \case
        Right a  -> return $ Right a
        Left err -> runStreamWithErrors $ handler err
