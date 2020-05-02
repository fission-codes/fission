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

type Errors = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App
   ]

class Monad m => Modifier m where
  updateCID :: UserId -> AppId -> CID -> UTCTime -> m (Either Errors ())

instance MonadIO m => Modifier (Transaction m) where
  updateCID userId appId newCID now =
    App.byId userId appId >>= \case
      Left err ->
        return $ Error.relaxedLeft err

      Right (Entity _ app) -> do
        if isOwnedBy userId app
          then do
            update appId [AppCid =. newCID]
            insert $ SetAppCIDEvent appId newCID now
            return ok
          else
            return . Error.openLeft $ ActionNotAuthorized @App userId
