module Main where

import Criterion
import Criterion.Main
import RIO

import qualified Fission.Internal.UTF8 as UTF8
import qualified RIO.ByteString.Lazy   as Lazy

main :: IO ()
main = defaultMain
  [ bgroup "Textable.encode" encode
  ]

encode :: [Benchmark]
encode =
  [ bench "Strict ByteString" $ whnf UTF8.encode ("hello world" :: ByteString)
  , bench "Lazy ByteString" $ whnf UTF8.encode ("hello world" :: Lazy.ByteString)
  ]
