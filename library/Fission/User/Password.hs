module Fission.User.Password
  ( random
  , hashPassword
  , module Fission.User.Password.Types
  , module Fission.User.Creator.Error
  ) where

import           Crypto.BCrypt hiding (hashPassword)

import           Fission.Prelude
import           Fission.Random as Random

import           Fission.User.Creator.Error
import qualified Fission.User.Creator.Error as User.Creator

import           Fission.User.Password.Types
import qualified Fission.User.Password.Types as User

-- | Generate a password for a `User`.
random :: MonadIO m => m User.Password
random = do
  pass <- Random.alphaNum 50
  return (User.Password pass)

hashPassword :: MonadIO m => Text -> m (Either User.Creator.Error Text)
hashPassword password = do
  password
    |> encodeUtf8
    |> hashPasswordUsingPolicy slowerBcryptHashingPolicy
    |> liftIO
    |> bind \case
      Nothing           -> return <| Left User.Creator.FailedDigest
      Just secretDigest -> return <| Right <| decodeUtf8Lenient secretDigest
