module Main (main) where

import           Test.Fission.Prelude
import qualified Test.Fission.Random

import qualified Test.Fission.Error as Error
import qualified Test.Fission.DNS as DNS

import qualified Test.Fission.User.DID as DID

import qualified Test.Fission.Web.Ping as Web.Ping
import qualified Test.Fission.Web.Auth as Web.Auth
import qualified Test.Fission.Web.Client as Web.Client

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "Fission Specs" <$> sequence
    [ Web.Auth.tests
    , Web.Client.tests
    , Web.Ping.tests
    , Error.tests
    , DID.tests
    , DNS.tests
    , Test.Fission.Random.tests
    ]
