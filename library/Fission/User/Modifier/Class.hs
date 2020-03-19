module Fission.User.Modifier.Class (Modifier (..)) where

import           Database.Persist
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Models
import           Fission.Prelude

import           Fission.IPFS.DNSLink as DNSLink
import           Fission.URL.Subdomain.Types
import           Fission.User.Username.Types

import           Fission.User.Password as Password
import           Fission.User.DID      as DID

class Monad m => Modifier m where
  updatePassword :: UserId -> Password   -> UTCTime -> m (Either Password.FailedDigest Password)
  updateDID      :: UserId -> DID        -> UTCTime -> m (DID)
  setData        :: UserId -> DID -> CID -> UTCTime -> m (Either ServerError ())

instance (MonadDNSLink m, MonadIO m) => Modifier (Transaction m) where
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

  updateDID userID did now = do
    update userID
      [ UserDid        =. Just did
      , UserModifiedAt =. now
      ]

    return did

  setData userId writerDID newCID now = do
    User {userUsername = Username username} <- updateGet userId
      [ UserDataRoot   =. newCID
      , UserModifiedAt =. now
      ]

    insert_ UpdateUserDataRootEvent
      { updateUserDataRootEventUserId      = userId
      , updateUserDataRootEventNewDataRoot = newCID
      , updateUserDataRootEventWriter      = writerDID
      , updateUserDataRootEventInsertedAt  = now
      }

    DNSLink.setBase (Subdomain username) newCID <&> \case
      Left err -> Left err
      Right _  -> ok
