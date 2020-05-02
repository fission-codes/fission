module Fission.User.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models

import           Fission.IPFS.DNSLink as DNSLink

import           Fission.Key           as Key
import           Fission.User.Password as Password

type Errors = OpenUnion
  '[ ServerError
   , NotFound User
   ]

class Monad m => Modifier m where
  updatePassword ::
       UserId
    -> Password
    -> UTCTime
    -> m (Either Password.FailedDigest Password)

  updatePublicKey ::
       UserId
    -> Key.Public
    -> UTCTime
    -> m Key.Public
   
  setData ::
       UserId
    -> CID
    -> UTCTime
    -> m (Either Errors ())

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

  updatePublicKey userID pk now = do
    update userID
      [ UserPublicKey  =. Just pk
      , UserModifiedAt =. now
      ]

    return pk

  setData userId newCID now = do
    update userId
      [ UserDataRoot   =. newCID
      , UserModifiedAt =. now
      ]

    insert_ UpdateUserDataRootEvent
      { updateUserDataRootEventUserId      = userId
      , updateUserDataRootEventNewDataRoot = newCID
      , updateUserDataRootEventInsertedAt  = now
      }

    return ok
