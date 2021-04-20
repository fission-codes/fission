module Fission.Test.CLI.Prelude
  ( module Fission.Prelude
  --
  , module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.Hspec.Wai
  , module Test.QuickCheck
  --
  , itsProp
  , itsProp'
  ) where

import qualified Network.HTTP.Types         as HTTP

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hspec

import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import           Test.Hspec.Wai             hiding (pending, pendingWith)

import           Test.QuickCheck            hiding (Result (..))
import           Test.QuickCheck.Instances  ()

import           Fission.Prelude            hiding (Result (..), log)

-- | Prop test with description
itsProp :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsProp description times prop =
  modifyMaxSuccess (\_ -> times) . it ("🔀 " <> description) $ property prop

-- | Prop test with the default number of tries (100)
itsProp' :: (HasCallStack, Testable a) => String -> a -> SpecWith ()
itsProp' description prop = it ("🔀 " <> description) $ property prop
