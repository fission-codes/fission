module Fission.Process.Status
  ( progress
  , module Fission.Process.Status.Types
  ) where

import           Fission.Prelude              hiding (Success)

import           Fission.Process.Status.Types

-- | The furthest status that any have reached (similar to a max function)
progress :: Foldable t => t (Status err a) -> Status err a
progress statuses = foldr combiner InProgress statuses
  where
    combiner _x           (Success a)  = Success a
    combiner (Success a)  _acc         = Success a
    combiner (Failed  _)  (Failed err) = Failed err
    combiner _x           _acc         = InProgress
