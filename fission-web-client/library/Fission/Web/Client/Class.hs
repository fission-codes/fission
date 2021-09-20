{-# LANGUAGE UndecidableInstances #-}

module Fission.Web.Client.Class (MonadWebClient (..), WebRequest (..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.Internal.Mock
import qualified Fission.Internal.Mock.Effect as Effect

class Monad m => MonadWebClient m where
  sendRequest :: ClientM a -> m (Either ClientError a)

-- | Effect for Mock instances
data WebRequest = WebRequest
  deriving (Show, Eq)
