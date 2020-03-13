module Fission.App.Creator.Class (Creator (..)) where

import Servant

import           Database.Esqueleto (insert)
import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.Error as Error
import           Fission.Models
import           Fission.URL

import           Fission.IPFS.DNSLink as DNSLink

import           Fission.Authorization

import qualified Fission.App.Domain.Associate.Class as AppDomain
import qualified Fission.App.Domain.Associate.Error as AppDomain

type Errors = OpenUnion
  '[ ServerError
   , AppDomain.AlreadyExists
   , Unauthorized
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))

instance (MonadDNSLink m, MonadIO m) => Creator (Transaction m) where
  create ownerId cid now = do
    appId <- insert App
      { appOwnerId    = ownerId
      , appCid        = cid
      , appInsertedAt = now
      , appModifiedAt = now
      }

    AppDomain.associateDefault ownerId appId now >>= \case
      Left err ->
        return . Left <| relaxOpenUnion err

      Right subdomain ->
        DNSLink.set (Just subdomain) cid <&> \case
          Left  err -> Error.openLeft err
          Right _   -> Right (appId, subdomain)
