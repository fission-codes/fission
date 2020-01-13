module Fission.User.Destroyer.Class (Destroyer (..)) where

import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude

-- | Destroy @User@s
class Monad m => Destroyer m where
  destroy :: UserId -> m ()

instance MonadIO m => Destroyer (Transaction m) where
  destroy userId =
    delete <| from \user ->
      where_ (user ^. UserId ==. val userId)
