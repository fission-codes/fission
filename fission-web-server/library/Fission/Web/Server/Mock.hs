module Fission.Web.Server.Mock
  ( module Fission.Web.Server.Mock.Types
  , module Fission.Web.Server.Mock.Effect
  , module Fission.Web.Server.Mock.Config
  , runMock
  , runMockIO'
  , runMockIO
  ) where

import           Control.Monad.Writer           (runWriterT)

import           Fission.Prelude

import           Fission.Web.Server.Mock.Types
import           Fission.Web.Server.Mock.Types  as Mock

import           Fission.Web.Server.Mock.Config
import           Fission.Web.Server.Mock.Effect

-- | Run the action described by a @Mock@
runMock :: forall effs m a . MonadIO m => Mock.Config -> Mock effs a -> m (Mock.Session effs a)
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
