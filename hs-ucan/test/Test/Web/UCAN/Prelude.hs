module Test.Web.UCAN.Prelude
  ( module RIO
  , module Data.Aeson

  --
  , module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.QuickCheck

  --
  , itsProp
  , itsPropSized
  , itsProp'
  ) where

import           RIO

import           Data.Aeson

import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.QuickCheck            hiding (Result (..))
import           Test.QuickCheck.Instances  ()
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hspec


-- | Prop test with description
itsProp :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsProp description times prop =
  modifyMaxSuccess (\_ -> times) . it ("🔀 " <> description) $ property prop

-- | Prop test with the default number of tries (100), but a different test case size
itsPropSized :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsPropSized description size prop =
  modifyMaxSize (\_ -> size) . it ("🔀 " <> description) $ property prop

-- | Prop test with the default number of tries (100)
itsProp' :: (HasCallStack, Testable a) => String -> a -> SpecWith ()
itsProp' description prop = it ("🔀 " <> description) $ property prop
