module Fission.User.Modifier.Class (Modifier (..)) where

import           Database.Persist
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Models
import           Fission.Prelude

import           Fission.IPFS.DNSLink as DNSLink
import           Fission.URL.Subdomain.Types
import           Fission.User.Username.Types

import           Fission.PublicKey.Types as PublicKey
import           Fission.User.Password   as Password

class Monad m => Modifier m where
  updatePassword ::
       UserId
    -> Password
    -> UTCTime
    -> m (Either Password.FailedDigest Password)

  updatePublicKey ::
       UserId
    -> PublicKey
    -> PublicKey.Algorithm
    -> UTCTime
    -> m PublicKey
   
  setData ::
       UserId
 --   -> PublicKey -- Record full credential in middleware logger
    -> CID
    -> UTCTime
    -> m (Either ServerError ())

instance (MonadDNSLink m, MonadIO m) => Modifier (Transaction m) where
  updatePassword userId password now =
    Password.hashPassword password >>= \case
      Left err ->
        return (Left err)

      Right secretDigest -> do
        update userId
          [ UserSecretDigest =. Just secretDigest
          , UserModifiedAt   =. now
          ]

        return (Right password)

  updatePublicKey userID pk algo now = do
    update userID
      [ UserPublicKey  =. Just pk
      , UserAlgorithm  =. Just algo
      , UserModifiedAt =. now
      ]

    return pk

  setData userId newCID now = do
    User {userUsername = Username username} <- updateGet userId
      [ UserDataRoot   =. newCID
      , UserModifiedAt =. now
      ]

    insert_ UpdateUserDataRootEvent
      { updateUserDataRootEventUserId      = userId
      , updateUserDataRootEventNewDataRoot = newCID
      , updateUserDataRootEventInsertedAt  = now
      }

    DNSLink.setBase (Subdomain username) newCID <&> \case
      Left err -> Left err
      Right _  -> ok
