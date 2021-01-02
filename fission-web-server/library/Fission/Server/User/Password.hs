module Fission.User.Password
  ( random
  , hashPassword
  , module Fission.User.Password.Types
  , module Fission.User.Password.Error
  ) where

import           Crypto.BCrypt hiding (hashPassword)

import           Fission.Prelude
import           Fission.Random as Random

import           Fission.User.Password.Types
import qualified Fission.User.Password.Types as User
import           Fission.User.Password.Error

-- | Generate a password for a `User`.
random :: MonadIO m => m User.Password
random = do
  pass <- Random.alphaNum 50
  return (User.Password pass)

hashPassword :: MonadIO m => User.Password -> m (Either FailedDigest Text)
hashPassword (User.Password password) = do
  password
    |> encodeUtf8
    |> hashPasswordUsingPolicy slowerBcryptHashingPolicy
    |> liftIO
    |> bind \case
      Nothing           -> return <| Left FailedDigest
      Just secretDigest -> return <| Right <| decodeUtf8Lenient secretDigest
