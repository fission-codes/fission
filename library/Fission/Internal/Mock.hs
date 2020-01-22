module Fission.Internal.Mock
  ( module Fission.Internal.Mock.Effect
  , module Fission.Internal.Mock.Types
  , runMock
  , runMockIO
  ) where

import           Control.Monad.Writer (runWriterT)

import           Fission.Internal.Mock.Types
import           Fission.Internal.Mock.Effect

import           Fission.Prelude

-- | Run the action described by a @Mock@
runMock :: MonadIO m => ctx -> Mock effs ctx a -> m (MockSession effs a)
runMock ctx action =
  action
    |> unMock
    |> runWriterT
    |> runRIO ctx
    |> fmap \(result, effectLog) -> MockSession {..}

runMockIO :: MonadIO m => ctx -> Mock effs ctx a -> m a
runMockIO ctx action =
  action
    |> unMock
    |> runWriterT
    |> runRIO ctx
    |> fmap fst
