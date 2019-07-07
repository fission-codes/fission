{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Storage.SQLite.Internal (traceAll) where

import RIO

import Database.Selda
import Database.Selda.SQLite

traceAll :: (Show a, Relational a) => Table a -> IO ()
traceAll tbl = withSQLite "ipfs-api.sqlite" do
  rows <- query (select tbl)
  forM_ rows (traceIO . textDisplay . displayShow)
