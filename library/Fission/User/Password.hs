module Fission.User.Password (random) where

import           Fission.Prelude
import           Fission.Random              as Random
import qualified Fission.User.Password.Types as User

{-| Generate a password for a `User`.
-}
random :: MonadIO m => m User.Password
random = do
  pass <- Random.alphaNum 50
  return (User.Password pass)
