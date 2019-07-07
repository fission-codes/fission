{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Web.Client (run) where

import RIO

import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client          (ClientM)

import qualified Fission.Web.Tls as Tls

run :: (Show a, HasLogFunc cfg) => String -> ClientM a -> RIO cfg ()
run loc clnt = do
  mgr  <- newTlsManager
  resp <- liftIO $ Tls.run mgr clnt loc

  case resp of
    Right result -> logInfo  $ displayShow result
    Left servErr -> logError $ displayShow servErr
