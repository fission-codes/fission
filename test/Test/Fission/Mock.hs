module Test.Fission.Mock
  ( module Test.Fission.Mock.Effect
  , module Test.Fission.Mock.Types
  , runMock
  ) where

import           Control.Monad.Writer
import           Data.Generics.Product
import qualified Network.IPFS.Types  as IPFS
import           Servant

import           Test.Hspec.Core.QuickCheck (modifyMaxSize)
import           Test.QuickCheck
import           Test.Tasty.Hspec

import           Test.Fission.Mock.Types
import           Test.Fission.Mock.Effect

import           Fission.IPFS.Linked.Class
import           Fission.Prelude
import           Fission.Web.Auth.Class

-- | Run the action described by a @Mock@
runMock :: MonadIO m => ctx -> Mock effs ctx a -> m (MockSession effs a)
runMock ctx action =
  action
    |> unMock
    |> runWriterT
    |> runRIO ctx
    |> fmap \(result, effectLog) -> MockSession {..}
