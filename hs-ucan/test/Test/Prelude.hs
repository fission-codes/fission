module Test.Prelude
  ( module RIO
  , module Data.Aeson

  --
  , module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.QuickCheck

  --
  , itsProp
  , itsProp'
  ) where

import           RIO

import           Data.Aeson

import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck            hiding (Result (..))
import           Test.QuickCheck.Instances  ()
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hspec


-- | Prop test with description
itsProp :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsProp description times prop =
  modifyMaxSuccess (\_ -> times) . it ("🔀 " <> description) $ property prop

-- | Prop test with the default number of tries (100)
itsProp' :: (HasCallStack, Testable a) => String -> a -> SpecWith ()
itsProp' description prop = it ("🔀 " <> description) $ property prop
