module Fission.App.Creator.Class (Creator (..)) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Models
import           Fission.URL.Subdomain.Types

import qualified Fission.App.Domain.Associate.Class as AppDomain
import qualified Fission.App.Domain.Associate.Error as AppDomain

type Errors = OpenUnion
  '[ AppDomain.AlreadyExists
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))

instance AppDomain.Associate m => Creator (Transaction m) where
  create ownerId cid now = do
    appId <- insert App
      { appOwnerId    = ownerId
      , appCid        = cid
      , appInsertedAt = now
      , appModifiedAt = now
      }

  AppDomain.associateDefault appId subdomain now
    <&> fmap \subdomain -> (appId, subdomain)
