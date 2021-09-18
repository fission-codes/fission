module Fission.Internal.Mock
  ( module Fission.Internal.Mock.Types
  , module Fission.Internal.Mock.Effect
  , runMock
  , runMockIO
  ) where

import           Control.Monad.Writer         (runWriterT)

import           Fission.Prelude

import           Fission.Internal.Mock.Effect
import           Fission.Internal.Mock.Types  as Mock

-- Reexports

import           Fission.Internal.Mock.Types

-- | Run the action described by a @Mock@
runMock :: MonadIO m => cfg -> Mock effs cfg a -> m (Mock.Session effs a)
runMock cfg action = do
  action
    |> unMock
    |> runWriterT
    |> runRIO cfg
    |> fmap \(result, effectLog) -> Mock.Session {..}

runMockIO :: MonadIO m => cfg -> Mock effs cfg a -> m a
runMockIO cfg (Mock action) = fmap fst . runRIO cfg $ runWriterT action
