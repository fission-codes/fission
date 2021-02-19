module Fission.Process.Status.Types (Status (..)) where

import           Fission.Prelude hiding (Success)

data Status err val
  = Failed err
  | InProgress
  | Success val
  deriving (Show, Eq, Ord)

instance (Display err, Display val) => Display (Status err val) where
  display = \case
    Failed err  -> "Failed with: " <> display err
    InProgress  -> "In progress"
    Success val -> "Succeeded with: " <> display val

instance Functor (Status err) where
  fmap f = \case
    Failed err  -> Failed err
    InProgress  -> InProgress
    Success val -> Success $ f val
