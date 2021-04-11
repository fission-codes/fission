module Main (main) where

import           RIO
import qualified RIO.ByteString.Lazy   as Lazy

import qualified Fission.Internal.UTF8 as UTF8

import           Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup "Textable.encode" encode
    ]

encode :: [Benchmark]
encode =
  [ bench "Strict ByteString" $ nf UTF8.encode ("hello world" :: ByteString)
  , bench "Lazy ByteString"   $ nf UTF8.encode ("hello world" :: Lazy.ByteString)
  ]
