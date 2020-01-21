module Test.Fission.Prelude
  ( module Fission.Prelude
  , module Test.Fission.Mock
  , module Test.Tasty
  , module Data.Generics.Product
  , itsProp
  ) where

import           Data.Generics.Product

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Hspec.Core.QuickCheck (modifyMaxSize)

import           Test.QuickCheck (Testable, property)
import           Test.QuickCheck.Instances ()

import           Test.Fission.Mock
import           Fission.Prelude hiding (Result (..))

itsProp :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsProp description times closure =
  modifyMaxSize (\_ -> times) <| it description <| property closure
