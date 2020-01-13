module Fission.User.Modifier.Class (Modifier (..)) where

import           Database.Persist

import           Fission.Models
import           Fission.Prelude

import qualified Fission.User.Creator.Error as Create
import           Fission.User.Password      as Password

class Monad m => Modifier m where
  updatePassword :: UserId -> Password -> UTCTime -> m (Either Create.Error Password) -- TODO wait, which error?

instance MonadIO m => Modifier (Transaction m) where
  updatePassword userId (Password password) now =
    Transaction <|
      Password.hashPassword password >>= \case
        Left err ->
          return (Left err)

        Right secretDigest -> do
          update userId
            [ UserSecretDigest =. secretDigest
            , UserModifiedAt   =. now
            ]

          return . Right <| Password password
