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

type Errors = OpenUnion
  '[ ServerError
   , AppDomain.AlreadyAssociated
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))

instance (MonadIO m, AppDomain.Initializer m) => Creator (Transaction m) where
  create ownerId cid now = do
    appId <- insert App
      { appOwnerId    = ownerId
      , appCid        = cid
      , appInsertedAt = now
      , appModifiedAt = now
      }

    AppDomain.associateDefault ownerId appId now <&> \case
      Left  err       -> Error.relaxedLeft err
      Right subdomain -> Right (appId, subdomain)
