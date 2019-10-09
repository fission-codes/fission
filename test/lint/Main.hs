module Main (main) where

import RIO

import Language.Haskell.HLint (hlint)

arguments :: [String]
arguments =
    [ "benchmark"
    , "fission-web"
    , "library"
    , "test/testsuite"
    ]

main :: IO ()
main = hlint arguments >> exitSuccess

-- main = do
  -- hints <- hlint arguments
  -- if null hints
  --   then exitSuccess
  --   else exitFailure
