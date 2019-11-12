module Fission.User.Password (random) where

import           Fission.Prelude
import           Fission.Random              as Random
import qualified Fission.User.Password.Types as User

random :: MonadIO m => m User.Password
random = do
  pass <- liftIO <| Random.text 200
  return <| User.Password pass
