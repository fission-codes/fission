module Fission.App.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Database.Esqueleto hiding ((<&>))

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Domain as App.Domain

type Errors = OpenUnion
  '[ ServerError
   , App.Domain.AlreadyAssociated

   , ActionNotAuthorized App
   , NotFound            App

   , ActionNotAuthorized URL
   , NotFound            URL

   , InvalidURL
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))

instance (MonadIO m, App.Domain.Initializer m) => Creator (Transaction m) where
  create ownerId cid now = do
    appId <- insert App
      { appOwnerId    = ownerId
      , appCid        = cid
      , appInsertedAt = now
      , appModifiedAt = now
      }

    App.Domain.associateDefault ownerId appId now <&> \case
      Left  err       -> Error.relaxedLeft err
      Right subdomain -> Right (appId, subdomain)
