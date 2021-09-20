module Fission.Test.CLI.Prelude
  ( module Fission.Prelude
  --
  , module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.QuickCheck
  --
  , itsProp'
  , shouldHaveRun
  ) where

import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hspec

import           Test.QuickCheck           hiding (Result (..))
import           Test.QuickCheck.Instances ()

import           Fission.Prelude           hiding (Result (..), log)

-- | Prop test with the default number of tries (100)
itsProp' :: (HasCallStack, Testable a) => String -> a -> SpecWith ()
itsProp' description prop = it ("🔀 " <> description) $ property prop

shouldHaveRun ::
  ( Eq   (OpenUnion logs)
  , Show (OpenUnion logs)
  , IsMember eff logs
  )
  => [OpenUnion logs]
  -> eff
  -> Expectation
shouldHaveRun effLog eff = effLog `shouldContain` [openUnionLift eff]
