module Fission.User.Destroyer.Class (Destroyer (..)) where

import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude hiding (set, Value)
import           Fission.Error as Error

-- | Destroy @User@s
class Monad m => Destroyer m where
  deactivate :: UserId -> UserId -> m (Either (ActionNotAuthorized User) ())

instance MonadIO m => Destroyer (Transaction m) where
  deactivate requestorId userId = do
    appCount :: [Value Int] <- select $ from \app -> do
      where_ (app ^. AppOwnerId ==. val userId)
      let n = countRows :: SqlExpr (Value Int)
      return n

    let
      cnt :: Int
      cnt = foldr (\(Value i) acc -> acc + i) 0 appCount

    if cnt <= 1 && requestorId == userId
      then do
        update \user -> do
          user `set` [ UserActive =. val False ]
          where_ (user ^. UserId ==. val userId)

        return ok

      else
        return . Left $ ActionNotAuthorized requestorId
