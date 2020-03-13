module Fission.App.Creator.Class (Creator (..)) where

import Servant

import           Database.Esqueleto (insert)
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Models
import           Fission.Models.Error
import           Fission.URL

import qualified Fission.Error as Error

import qualified Fission.App.Domain.Associate as AppDomain
import           Fission.IPFS.DNSLink         as DNSLink

type Errors = OpenUnion
  '[ ServerError
   , AppDomain.AlreadyExists
   , ActionNotAuthorized App
   , NotFound            App
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
        cid
          |> DNSLink.setBase (Just subdomain) -- FIXME
          |> fmap \case
            Left  err -> Error.openLeft err
            Right _   -> Right (appId, subdomain)
