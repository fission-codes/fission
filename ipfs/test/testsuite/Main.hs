{-# LANGUAGE NoImplicitPrelude #-}

-- {-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import RIO

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit             (Assertion, testCase, (@?=))
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)
import Test.Tasty.SmallCheck        (testProperty)

-- import Test.Hspec
-- import Test.Hspec.Wai
-- import Test.Hspec.Wai.JSON

-- import Network.Wai.Handler.Warp (run)

-- main :: IO ()
-- main = return ()

-- main = do
--   -- port <- lookupSetting "PORT" 8081
--   -- pool <- makePool Test

--   -- runSqlPool doMigrate pool

--   -- let logger = setLogger Test
--   --     config = Config { getPool = pool
--   --                     , getEnv  = Test
--   --                     }

--   putStrLn "Testing..."
--   hspec . spec . run port . logger $ app config

-- spec :: IO () -> Spec
-- spec server = with server do
--   describe "GET /ping" $ do
--     it "responds with 200" do
--       1 `shouldBe` 1
--       -- get "/users" `shouldRespondWith` 200

--         -- it "responds with 'Simple'" do
--         --     get "/" `shouldRespondWith` "Simple"

-- config = Config { getPool = , getEnv = Test }

-- main :: IO ()
-- main = return ()

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "all-tests" tests)

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  -- , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ testProperty "inc == succ" prop_succ
  , testProperty "inc . negate == negate . pred" prop_pred
  ]

-- huTests :: [TestTree]
-- huTests =
--   [ testCase "Increment below TheAnswer" case_inc_below
--   , testCase "Decrement above TheAnswer" case_dec_above
--   ]

prop_succ :: Int -> Bool
prop_succ n = 1 + n == 1 + n

prop_pred :: Int -> Bool
prop_pred n = 1 + (negate n) == negate (n - 1)

-- case_inc_below :: Assertion
-- case_inc_below = inc 41 @?= (42 :: Int)

-- case_dec_above :: Assertion
-- case_dec_above = negate (inc (negate 43)) @?= (42 :: Int)
