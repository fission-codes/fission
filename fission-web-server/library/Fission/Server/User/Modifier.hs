module Fission.User.Modifier
  ( updatePasswordDB
  , updatePublicKeyDB
  , addExchangeKeyDB
  , removeExchangeKeyDB
  , setDataDB
  , module Fission.User.Modifier.Class
  ) where

import           Fission.User.Modifier.Class

import           Fission.Error
import           Fission.Models
import           Fission.Prelude

import qualified Crypto.PubKey.RSA           as RSA
import qualified Fission.Key                 as Key
import           Fission.Security.Types

import           Database.Persist            as Persist

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import qualified RIO.List                    as List


updatePasswordDB ::
     MonadIO m
  => UserId
  -> SecretDigest
  -> UTCTime
  -> Transaction m ()
updatePasswordDB userId secretDigest now =
  update userId
    [ UserSecretDigest =. Just secretDigest
    , UserModifiedAt   =. now
    ]

updatePublicKeyDB ::
     MonadIO m
  => UserId
  -> Key.Public
  -> UTCTime
  -> Transaction m (Either Errors' Key.Public)
updatePublicKeyDB userID pk now = do
  update userID
    [ UserPublicKey  =. Just pk
    , UserModifiedAt =. now
    ]

  return $ Right pk

addExchangeKeyDB ::
     MonadIO m
  => UserId
  -> RSA.PublicKey
  -> UTCTime
  -> Transaction m (Either Errors' [RSA.PublicKey])
addExchangeKeyDB userID key now =
  Persist.get userID >>= \case
    Nothing ->
      return . openLeft $ NotFound @User

    Just User {userExchangeKeys = Nothing} -> do
      update userID
        [ UserExchangeKeys =. Just [key]
        , UserModifiedAt   =. now
        ]

      return $ Right [key]

    Just User {userExchangeKeys = Just keys} -> do
      let
        updated = key : keys

      if List.elem key keys
        then
          return $ Right keys

        else do
          update userID
            [ UserExchangeKeys =. Just updated
            , UserModifiedAt   =. now
            ]

          return $ Right updated

removeExchangeKeyDB ::
    MonadIO m
  => UserId
  -> RSA.PublicKey
  -> UTCTime
    -> Transaction m (Either Errors' [RSA.PublicKey])
removeExchangeKeyDB userID key now =
  Persist.get userID >>= \case
    Nothing ->
      return . openLeft $ NotFound @User

    Just User {userExchangeKeys = Nothing} ->
      return $ Right []

    Just User {userExchangeKeys = Just keys} ->
      let
        updated = List.delete key keys

      in
        if List.elem key keys
          then do
            update userID
              [ UserExchangeKeys =. Just updated
              , UserModifiedAt   =. now
              ]
            return $ Right updated

          else
            return $ Right keys

setDataDB ::
     MonadIO m
  => UserId
  -> CID
  -> Bytes
  -> UTCTime
  -> Transaction m ()
setDataDB userId newCID size now = do
  update userId
    [ UserDataRoot     =. newCID
    , UserDataRootSize =. size
    , UserModifiedAt   =. now
    ]

  insert_ $ UpdateUserDataRootEvent userId newCID size now
