module Fission.Internal.Mock.Effect (log) where

import           Control.Monad.Writer
import           Fission.Prelude

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
