module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Models
import           Fission.Models.Error
import           Fission.Ownership

import qualified Fission.Error as Error

import qualified Fission.App.Retriever.Class as App
import           Fission.IPFS.DNSLink.Class  as DNSLink

type Errors = OpenUnion
  '[ NotFound App
   , ActionNotAuthorized App
   ]

class Monad m => Modifier m where
  updateCID :: UserId -> AppId -> CID -> UTCTime -> m (Either Errors ())

instance MonadDNSLink m => Modifier (Transaction m) where
  updateCID userId appId newCID now =
    App.byId appId >>= \case
      Left err ->
        return <| Error.openLeft err

      Right (Entity _ app) -> do
        if isOwnedBy userId app
          then do
            update appId [AppCid =. newCID]
            insert (SetAppCIDEvent appId newCID now)

            -- Update DNS records for each registered AppDomain
            appDomains <- selectList [AppDomainAppId ==. appId] []
            forM_ appDomains \(Entity _ AppDomain {..}) ->
              DNSLink.set appDomainDomainName appDomainSubdomain newCID

            return ok

          else
            return . Error.openLeft <| ActionNotAuthorized @App userId
