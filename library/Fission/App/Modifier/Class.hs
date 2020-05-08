module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist       as Persist

import           Network.IPFS.Add.Error as IPFS.Pin
import           Network.IPFS.CID.Types

import           Servant.Server

import           Fission.Models
import           Fission.Ownership
import           Fission.Prelude        hiding (on)
import           Fission.URL.Types

import           Fission.Error          as Error

type Errors = OpenUnion
  '[ NotFound App
   , NotFound AppDomain
   , NotFound Domain
   , NotFound URL

   , ActionNotAuthorized App
   , ActionNotAuthorized AppDomain
   , ActionNotAuthorized URL

   , IPFS.Pin.Error
   , ServerError
   ]

class Monad m => Modifier m where
  setCID ::
       UserId  -- ^ User for auth
    -> URL     -- ^ URL associated with target app
    -> CID     -- ^ New CID
    -> Bool    -- ^ Flag: copy data (default yes)
    -> UTCTime -- ^ Now
    -> m (Either Errors AppId)

instance MonadIO m => Modifier (Transaction m) where
  setCID userId URL {..} newCID _copyFlag now = do
    mayAppDomain <- Persist.selectFirst
      [ AppDomainDomainName ==. domainName
      , AppDomainSubdomain  ==. subdomain
      ] []

    case mayAppDomain of
      Nothing ->
        return . Error.openLeft $ NotFound @AppDomain

      Just (Entity _ AppDomain {appDomainAppId = appId}) ->
        Persist.get appId >>= \case
          Nothing ->
            return . Error.openLeft $ NotFound @App

          Just app ->
            if isOwnedBy userId app
              then do
                update appId [AppCid =. newCID]
                insert $ SetAppCIDEvent appId newCID now
                return $ Right appId

              else
                return . Error.openLeft $ ActionNotAuthorized @App userId
