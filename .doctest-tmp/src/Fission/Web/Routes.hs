{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Web.Routes
  ( API
  , IPFSRoute
  , HerokuRoute
  , PingRoute
  ) where

import RIO

import Servant

import           Fission.User
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Heroku as Heroku

type API = IPFSRoute :<|> HerokuRoute :<|> PingRoute

type IPFSRoute = "ipfs"
                 :> BasicAuth "registered users" User
                 :> IPFS.API

type HerokuRoute = "heroku"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type PingRoute = "ping" :> Ping.API
