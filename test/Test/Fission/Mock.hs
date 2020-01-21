module Test.Fission.Mock
  ( module Test.Fission.Mock.Effect
  , module Test.Fission.Mock.Types
  , runMock
  ) where

import           Control.Monad.Writer (runWriterT)

import           Test.Fission.Mock.Types
import           Test.Fission.Mock.Effect

import           Fission.Prelude

-- | Run the action described by a @Mock@
runMock :: MonadIO m => ctx -> Mock effs ctx a -> m (MockSession effs a)
runMock ctx action =
  action
    |> unMock
    |> runWriterT
    |> runRIO ctx
    |> fmap \(result, effectLog) -> MockSession {..}
