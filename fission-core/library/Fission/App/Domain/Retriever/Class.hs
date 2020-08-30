module Fission.App.Domain.Retriever.Class
  ( Retriever (..)
  , Errors'
  ) where

import           Database.Esqueleto          hiding ((<&>))
import qualified Database.Persist            as P

import           Fission.Error
import           Fission.Models
import           Fission.Prelude             hiding (on)
import           Fission.URL

import qualified Fission.App.Retriever.Class as App

type Errors' = OpenUnion
  '[ NotFound            AppDomain
   , ActionNotAuthorized AppDomain

   , NotFound            App
   , ActionNotAuthorized App
   ]

class Monad m => Retriever m where
  allForOwner         :: UserId -> m [Entity AppDomain]
  allForApp           :: AppId  -> m [Entity AppDomain]
  allSiblingsByDomain :: DomainName -> Maybe Subdomain -> m (Either Errors' [Entity AppDomain])
  primarySibling      :: UserId -> URL -> m (Either Errors' (Entity AppDomain))

instance MonadIO m => Retriever (Transaction m) where
  primarySibling userId url = do
    App.byURL userId url >>= \case
      Left err ->
        return $ relaxedLeft err

      Right (Entity appId _) ->
        getBy (UniquePrimaryForApp appId Active) <&> \case
          Nothing        -> openLeft $ NotFound @AppDomain
          Just appDomain -> Right appDomain

  allForApp appId = P.selectList [AppDomainAppId P.==. appId] []

  allSiblingsByDomain domainName maySubdomain = do
    mayAppDomain <- P.selectFirst
      [ AppDomainDomainName P.==. domainName
      , AppDomainSubdomain  P.==. maySubdomain
      ]
      []

    case mayAppDomain of
      Nothing ->
        return . openLeft $ NotFound @AppDomain

      Just (Entity _ AppDomain {appDomainAppId}) ->
        Right <$> P.selectList [AppDomainAppId P.==. appDomainAppId] []

  allForOwner ownerId = do
    select $ from \(app `InnerJoin` appDomain) -> do
      on $ app       ^. AppOwnerId     ==. val ownerId
       &&. appDomain ^. AppDomainAppId ==. app ^. AppId

      return appDomain
