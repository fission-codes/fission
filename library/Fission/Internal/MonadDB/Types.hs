module Fission.Internal.MonadDB.Types (Transaction (..)) where

import Control.Monad.Logger
import Database.Persist.Sql
import RIO

newtype Transaction m a = Transaction { unTransaction :: ReaderT SqlBackend m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader SqlBackend
                   , MonadLogger
                   , MonadThrow
                   , MonadTrans
                   )
