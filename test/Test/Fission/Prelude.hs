module Test.Fission.Prelude
  ( module Fission.Prelude
  , module Test.Fission.Mock
  , module Test.Tasty
  , module Data.Generics.Product
  , bodyMatches
  , itsProp
  , itsProp'
  ) where

import           Data.Generics.Product
import qualified Network.HTTP.Types as HTTP
import           Servant.QuickCheck

import           Test.Tasty
import           Test.Tasty.Hspec

import           Test.Hspec.Core.QuickCheck (modifyMaxSize)
import           Test.Hspec.Wai

import           Test.QuickCheck (Testable, property)
import           Test.QuickCheck.Instances ()

import           Test.Fission.Mock
import           Fission.Prelude hiding (Result (..))

itsProp :: (HasCallStack, Testable a) => String -> Int -> a -> SpecWith ()
itsProp description times prop =
  modifyMaxSize (\_ -> times) <| it description <| property prop

itsProp' :: (HasCallStack, Testable a) => String -> a -> SpecWith ()
itsProp' description prop = itsProp description 10_000 prop

bodyMatches :: Value -> [HTTP.Header] -> Body -> Maybe String
bodyMatches expected _ body =
  case decode body of -- Here success is Nothing, and errors are Just
      Just val | val == expected -> Nothing
      _                          -> Just "Body does not match pong"
