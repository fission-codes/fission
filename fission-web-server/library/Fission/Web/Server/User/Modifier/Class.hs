module Fission.Web.Server.User.Modifier.Class
  ( Modifier (..)
  , Errors'
  ) where

import qualified Crypto.PubKey.RSA                                  as RSA

import           Servant.Server

import qualified Network.IPFS.Add.Error                             as IPFS.Pin
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Get.Error                             as IPFS.Stat

import           Fission.Prelude

import           Fission.Error
import qualified Fission.Key                                        as Key
import           Fission.URL

import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import qualified Fission.Web.Server.HTTP.Cache.Error                as HTTP.Cache
import           Fission.Web.Server.Models
import           Fission.Web.Server.User.Password                   as Password

type Errors' = OpenUnion
  '[ NotFound User

   , NotFound            URL
   , ActionNotAuthorized URL

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
       UserId
    -> CID
    -> UTCTime
    -> m (Either Errors' ())
