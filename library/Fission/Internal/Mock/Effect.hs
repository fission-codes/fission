module Fission.Internal.Mock.Effect
  ( module Fission.Internal.Mock.Effect.Types
  , log
  ) where

import           Control.Monad.Writer

import           Fission.Prelude hiding (log)
import           Fission.Internal.Mock.Effect.Types

log ::
  ( IsMember eff log
  , Applicative t
  , MonadWriter (t (OpenUnion log)) m
  )
  => eff
  -> m ()
log effect =
  effect
    |> openUnionLift
    |> pure
    |> tell
