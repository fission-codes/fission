module Fission.App.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Database.Esqueleto (insert)
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Domain   as AppDomain
import           Fission.IPFS.DNSLink as DNSLink

type Errors = OpenUnion
  '[ ServerError
   , AppDomain.AlreadyAssociated
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))

instance (AppDomain.Initializer m, MonadDNSLink m) => Creator (Transaction m) where
  create ownerId cid now = do
    appId <- insert App
      { appOwnerId    = ownerId
      , appCid        = cid
      , appInsertedAt = now
      , appModifiedAt = now
      }

    AppDomain.associateDefault ownerId appId now >>= \case
      Left err ->
        return $ Error.relaxedLeft err

      Right subdomain ->
        DNSLink.setBase subdomain cid <&> \case
          Left  err -> Error.openLeft err
          Right _   -> Right (appId, subdomain)
