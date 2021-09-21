module Fission.Web.Server.User.Modifier
  ( updatePasswordDB
  , updateDidDB
  , addExchangeKeyDB
  , removeExchangeKeyDB
  , setDataDB
  , module Fission.Web.Server.User.Modifier.Class
  ) where

import qualified Crypto.PubKey.RSA                      as RSA

import qualified RIO.List                               as List

import           Database.Persist                       as Persist

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import           Fission.Prelude

import qualified Fission.Key                            as Key
import           Fission.Security.Types
import           Fission.User.DID.Types                 as DID

import           Fission.Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types
import           Fission.Web.Server.User.Modifier.Class

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

updateDidDB ::
     MonadIO m
  => UserId
  -> DID
  -> UTCTime
  -> Transaction m (Either Errors' DID)
updateDidDB userID did now = do
  case did of
    DID.Key pk ->
      update userID
        [ UserPublicKey  =. Just pk
        , UserModifiedAt =. now
        ]

    DID.ION ion ->
      update userID
        [ UserIon        =. Just ion
        , UserModifiedAt =. now
        ]

  return $ Right did

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
