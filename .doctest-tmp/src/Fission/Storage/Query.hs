{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Storage.Query
  ( is'
  , all'
  , getOne
  ) where

import RIO
import RIO.List (headMaybe)

import Database.Selda

is' :: Row s t -> Selector t Bool -> Col s Bool
is' row prop = row ! prop .== true

all' :: Row s t -> [Row s t -> Col s Bool] -> Col s Bool
all' row = foldr (\prop acc -> acc .&& prop row) true

getOne :: Functor f => f [a] -> f (Maybe a)
getOne = fmap headMaybe
