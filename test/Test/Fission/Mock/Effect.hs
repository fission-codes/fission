module Test.Fission.Mock.Effect
  ( logEff
  , shouldHaveRun
  ) where

import           Control.Monad.Writer
import           Test.Tasty.Hspec

import           Fission.Prelude

logEff ::
  ( IsMember eff log
  , Applicative t
  , MonadWriter (t (OpenUnion log)) m
  )
  => eff
  -> m ()
logEff effect =
  effect
    |> openUnionLift
    |> pure
    |> tell

shouldHaveRun ::
  ( Eq   (OpenUnion logs)
  , Show (OpenUnion logs)
  , IsMember eff logs
  )
  => [OpenUnion logs]
  -> eff
  -> Expectation
shouldHaveRun effLog eff = effLog `shouldContain` [openUnionLift eff]
