module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Models
import           Fission.Ownership

import qualified Fission.App.Retriever as App
import           Fission.Error         as Error
import           Fission.IPFS.DNSLink  as DNSLink

type Errors = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App
   ]

class Monad m => Modifier m where
  updateCID :: UserId -> AppId -> CID -> UTCTime -> m (Either Errors ())

instance MonadDNSLink m => Modifier (Transaction m) where
  updateCID userId appId newCID now =
    App.byId userId appId >>= \case
      Left err ->
        return <| Error.relaxedLeft err

      Right (Entity _ app) -> do
        if isOwnedBy userId app
          then do
            update appId [AppCid =. newCID]
            insert <| SetAppCIDEvent appId newCID now
            updateAssociatedDNS appId newCID
            return ok
          else
            return . Error.openLeft <| ActionNotAuthorized @App userId

-- | Update DNS records for each registered @AppDomain@
updateAssociatedDNS ::
  ( MonadIO                   m
  , MonadDNSLink (Transaction m)
  )
  => AppId
  -> CID
  -> Transaction m ()
updateAssociatedDNS appId newCID = do
  appDomains <- selectList [AppDomainAppId ==. appId] []
  forM_ appDomains \(Entity _ AppDomain {..}) ->
    DNSLink.set appDomainDomainName appDomainSubdomain newCID
