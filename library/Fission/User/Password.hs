module Fission.User.Password (random, hashPassword) where

import Crypto.BCrypt hiding (hashPassword)

import           Fission.Prelude
import           Fission.Random              as Random
import qualified Fission.User.Password.Types as User
import qualified Fission.User.Mutation.Error as Error

-- | Generate a password for a `User`.
random :: MonadIO m => m User.Password
random = do
  pass <- Random.alphaNum 50
  return (User.Password pass)

hashPassword :: MonadIO m => Text -> m (Either Error.Create Text)
hashPassword password = do
  hashed <- liftIO <| hashPasswordUsingPolicy slowerBcryptHashingPolicy <| encodeUtf8 password
  return <| case hashed of
    Nothing           -> Left Error.FailedDigest
    Just secretDigest -> Right <| decodeUtf8Lenient secretDigest
