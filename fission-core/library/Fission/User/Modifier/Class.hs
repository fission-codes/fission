module Fission.User.Modifier.Class
  ( Modifier (..)
  , Errors'
  ) where

import qualified Crypto.PubKey.RSA             as RSA

import           Servant.Server

import qualified Network.IPFS.Add.Error        as IPFS.Pin
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Get.Error        as IPFS.Stat

import           Fission.Prelude

import           Fission.Error
import           Fission.Models

import qualified Fission.Key                   as Key

import           Fission.URL
import           Fission.User.Password         as Password
import           Fission.User.Username.Types

import           Fission.WNFS.Permission.Types as WNFS

type Errors' = OpenUnion
  '[ NotFound User

   , NotFound          URL
   , UserNotAuthorized URL

   , ActionNotAuthorized User
   , UserNotAuthorized   User

   , IPFS.Pin.Error
   , IPFS.Stat.Error

   , ServerError
   , InvalidURL
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
    -> m (Either Errors' Key.Public)

  addExchangeKey ::
       UserId
    -> RSA.PublicKey
    -> UTCTime
    -> m (Either Errors' [RSA.PublicKey])

  removeExchangeKey ::
       UserId
    -> RSA.PublicKey
    -> UTCTime
    -> m (Either Errors' [RSA.PublicKey])

  setData ::
       Username
    -> CID
    -> UTCTime
    -> m (Either Errors' ())
