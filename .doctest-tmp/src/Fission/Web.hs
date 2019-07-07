{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

module Fission.Web
  ( API
  , app
  , server
  ) where

import RIO
import RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Data.Has
import Database.Selda
import Servant
import Data.Swagger as Swagger

import           Fission
import           Fission.User
import           Fission.Web.Server
import qualified Fission.IPFS.Types as IPFS

import qualified Fission.Web.Auth    as Auth
import qualified Fission.Web.IPFS    as IPFS
import qualified Fission.Web.Ping    as Ping
import qualified Fission.Web.Routes  as Web
import qualified Fission.Web.Swagger as Web.Swagger
import qualified Fission.Web.Types   as Web

import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Web.Heroku            as Heroku

type API = Web.Swagger.API :<|> Web.API

app :: Has IPFS.BinPath    cfg
    => Has Web.Host        cfg
    => Has Heroku.ID       cfg
    => Has Heroku.Password cfg
    => HasProcessContext   cfg
    => HasLogFunc          cfg
    => MonadSelda     (RIO cfg)
    =>     cfg
    -> RIO cfg Application
app cfg = do
  auth             <- mkAuth
  Web.Host appHost <- fromConfig
  return . serveWithContext api auth
         . Auth.server api cfg
         $ server (Swagger.Host (Text.unpack appHost) Nothing)

mkAuth :: Has Heroku.ID       cfg
       => Has Heroku.Password cfg
       => HasLogFunc          cfg
       => MonadSelda     (RIO cfg)
       => RIO cfg (Context '[BasicAuthCheck User, BasicAuthCheck ByteString])
mkAuth = do
  Heroku.ID       hkuID   <- fromConfig
  Heroku.Password hkuPass <- fromConfig

  let hku = Auth.basic hkuID hkuPass
  usr <- Auth.user

  return $ usr
        :. hku
        :. EmptyContext

server :: Has IPFS.BinPath  cfg
       => Has Web.Host      cfg
       => HasProcessContext cfg
       => HasLogFunc        cfg
       => MonadSelda   (RIO cfg)
       => Swagger.Host
       -> RIOServer         cfg API
server host' = Web.Swagger.server host'
          :<|> const IPFS.server
          :<|> const Heroku.create
          :<|> pure Ping.pong

api :: Proxy API
api = Proxy
