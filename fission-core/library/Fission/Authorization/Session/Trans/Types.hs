module Fission.Authorization.Session.Trans.Types where

import           Control.Concurrent.STM.TVar

import           Fission.Models

import           Fission.Prelude

import           Fission.User.Namespace.Class

import           Fission.App                                 as App
import           Fission.App.Domain                          as App.Domain
import           Fission.Domain                              as Domain
import           Fission.WNFS.Subgraph.Types                 as Subgraph

import           Fission.Web.Auth.Token.UCAN.Privilege.Types as Privilege

import           Fission.Authorization.Allowable
import           Fission.Authorization.Grantable

import           Fission.Authorization.Session.Class
import           Fission.Authorization.Session.Types

newtype SessionT m a = SessionT
  { unSessionT :: ReaderT (TVar Session) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Alternative
                   , Monad
                   , MonadIO
                   , MonadReader (TVar Session)
                   , MonadFail
                   , MonadTrans
                   )

runSessionT :: MonadIO m => Session -> SessionT m a -> m a
runSessionT session action = do
  sessionVar <- atomically $ newTVar session
  runReaderT (unSessionT action) sessionVar

instance MonadUserNamespace m => MonadUserNamespace (SessionT m) where
  getUserNamespace = lift getUserNamespace

instance App.Retriever m => App.Retriever (SessionT m) where
  byId    userId appId = lift $ byId    userId appId
  byURL   userId url   = lift $ byURL   userId url
  ownedBy userId       = lift $ ownedBy userId

instance Domain.Retriever m => Domain.Retriever (SessionT m) where
  getByDomainName = lift . getByDomainName

instance App.Domain.Retriever m => App.Domain.Retriever (SessionT m) where
  allForApp      appId      = lift $ allForApp      appId
  allForOwner    userId     = lift $ allForOwner    userId
  primarySibling userId url = lift $ primarySibling userId url

  allSiblingsByDomain domainName maySubdomain =
    lift $ allSiblingsByDomain domainName maySubdomain

instance
  ( MonadIO              m
  , App.Retriever        m
  , App.Domain.Retriever m
  , Domain.Retriever     m
  , MonadUserNamespace   m
  ) => MonadAuthSession App (SessionT m) where
  allChecked =
    queryAuthSession apps

  allUnchecked =
    foldr folder [] <$> queryAuthSession unchecked
    where
      folder priv acc =
        case priv of
          Privilege.FissionWebApp priv `DelegatedBy` userId ->
            (priv `DelegatedBy` userId) : acc

          AsUser uID ->
            AsUser uID : acc

          _ ->
            acc

  addAccess newPermission =
    modifyAuthSession \session@Session {apps} ->
      session {apps = (newPermission : apps)}

  dropUnchecked privilege userID =
    modifyAuthSession \session@Session {unchecked} ->
      session {unchecked = filter (toRemove (Privilege.FissionWebApp privilege) userID) unchecked}

instance
  ( MonadIO            m
  , Domain.Retriever   m
  , MonadUserNamespace m
  ) => MonadAuthSession Domain (SessionT m) where
  allChecked =
    queryAuthSession domains

  allUnchecked =
    foldr folder [] <$> queryAuthSession unchecked
    where
      folder priv acc =
        case priv of
          AsUser uID ->
            AsUser uID : acc

          Privilege.RegisteredDomain priv `DelegatedBy` userId ->
            (priv `DelegatedBy` userId) : acc

          _ ->
            acc

  addAccess newPermission =
    modifyAuthSession \session@Session {domains} ->
      session {domains = (newPermission : domains)}

  dropUnchecked privilege userID =
    modifyAuthSession \session@Session {unchecked} ->
      session {unchecked = filter (toRemove (Privilege.RegisteredDomain privilege) userID) unchecked}

instance
  ( MonadIO            m
  , Domain.Retriever   m
  , MonadUserNamespace m
  ) => MonadAuthSession Subgraph (SessionT m) where
  allChecked =
    queryAuthSession subgraphs

  allUnchecked =
    foldr folder [] <$> queryAuthSession unchecked
    where
      folder priv acc =
        case priv of
          Privilege.WNFS priv `DelegatedBy` userId ->
            (priv `DelegatedBy` userId) : acc

          AsUser uID ->
            AsUser uID : acc

          _ ->
            acc

  addAccess newPermission =
    modifyAuthSession \session@Session {subgraphs} ->
      session {subgraphs = (newPermission : subgraphs)}

  dropUnchecked privilege userID =
    modifyAuthSession \session@Session {unchecked} ->
      session {unchecked = filter (toRemove (Privilege.WNFS privilege) userID) unchecked}

queryAuthSession :: MonadIO m => (Session -> a) -> SessionT m a
queryAuthSession query = do
  sessionVar <- ask
  session    <- liftIO . atomically $ readTVar sessionVar
  return $ query session

modifyAuthSession ::
  MonadIO m
  => (Session -> Session)
  -> SessionT m ()
modifyAuthSession modifier = do
  sessionVar <- ask
  liftIO . atomically $ modifyTVar sessionVar modifier

toRemove ::
     Privilege
  -> UserId
  -> Unchecked Privilege
  -> Bool
toRemove privilege userId = \case
  priv `DelegatedBy` Entity uID _ -> privilege `isEncompassedBy` priv && uID == userId
  _ -> False
