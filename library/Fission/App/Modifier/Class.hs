module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Network.IPFS.CID.Types

import           Database.Esqueleto as SQL

import           Fission.Prelude
import           Fission.Models
import           Fission.Models.Error

import qualified Fission.Error as Error

type Errors = OpenUnion
  '[ NotFound App
   ]

class Monad m => Modifier m where
  updateCID :: AppId -> CID -> UTCTime -> m (Either Errors ())

instance MonadIO m => Modifier (Transaction m) where
  updateCID appId newCID now = do
    n <- updateCount \app -> do
      SQL.set app [AppCid =. val newCID]
      where_ (app ^. AppId ==. val appId)

    case n of
      0 ->
        return (Error.openLeft <| NotFound @App)

      _ -> do
        insert (SetAppCIDEvent appId newCID now)
        -- FIXME: update DNS
        return ok
