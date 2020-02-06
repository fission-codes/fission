module Fission.User.Modifier.Class (Modifier (..)) where

import           Database.Persist

import           Fission.Models
import           Fission.Prelude

import           Fission.User.Password as Password
import           Fission.User.DID      as DID

class Monad m => Modifier m where
  updatePassword :: UserId -> Password -> UTCTime -> m (Either Password.FailedDigest Password)
  updateDID      :: UserId -> DID -> UTCTime -> m (DID)

instance MonadIO m => Modifier (Transaction m) where
  updatePassword userId (Password password) now =
    Password.hashPassword password >>= \case
      Left err ->
        return (Left err)

      Right secretDigest -> do
        update userId
          [ UserSecretDigest =. Just secretDigest
          , UserModifiedAt   =. now
          ]

        return . Right <| Password password

  updateDID userID (DID did) now = do
    update userID
      [ UserDid          =. Just did
      , UserModifiedAt   =. now
      ]
    return <| DID did
