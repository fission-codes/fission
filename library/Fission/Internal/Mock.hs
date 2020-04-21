module Fission.Internal.Mock
  ( module Fission.Internal.Mock.Types
  , module Fission.Internal.Mock.Effect
  , module Fission.Internal.Mock.Config
  , runMock
  , runMockIO'
  , runMockIO
  ) where

import           Control.Monad.Writer (runWriterT)

import           Fission.Prelude

import           Fission.Internal.Mock.Types as Mock
import           Fission.Internal.Mock.Types

import           Fission.Internal.Mock.Config
import           Fission.Internal.Mock.Effect

-- | Run the action described by a @Mock@
runMock :: MonadIO m => Mock.Config -> Mock effs a -> m (Mock.Session effs a)
runMock cfg action = do
  action
    |> unMock
    |> runWriterT
    |> runRIO cfg
    |> fmap \(result, effectLog) -> Mock.Session {..}

runMockIO' :: Mock effs a -> IO a
runMockIO' = runMockIO defaultConfig

runMockIO :: MonadIO m => Mock.Config -> Mock effs a -> m a
runMockIO cfg action = do
  action
    |> unMock
    |> runWriterT
    |> runRIO cfg
    |> fmap fst
