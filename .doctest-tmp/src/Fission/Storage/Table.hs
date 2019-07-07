{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Storage.Table
  ( lensPrefixed
  , Name (..)
  ) where

import           RIO
import qualified RIO.Partial as Partial
import           RIO.Text    (stripPrefix)

import Database.Selda

newtype Name a = Name { name :: TableName }
  deriving         (Eq, Show)
  deriving newtype IsString

lensPrefixed :: Relational r => TableName -> [Attr r] -> Table r
lensPrefixed tableName conf =
  tableFieldMod tableName conf (Partial.fromJust . stripPrefix "_")
