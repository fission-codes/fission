module Test.Random (tests) where

import qualified RIO.ByteString as BS
import           Test.Tasty
import           Test.Tasty.HUnit

-- ⚛

import qualified Fission.Random as Random


tests :: TestTree
tests = testGroup
  "Random"
  [ testCase "bsRandomLength should be of the given length" do
      alphaNum <- Random.bsRandomLength 10
      BS.length alphaNum @?= 10

  ]
