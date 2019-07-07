{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Web.Types
  ( Host (..)
  , Port (..)
  ) where

import RIO

import qualified Network.Wai.Handler.Warp as Warp
import           System.Envy

newtype Host = Host { getHost :: Text }
  deriving         (Eq, Show)
  deriving newtype IsString

newtype Port = Port { port :: Warp.Port }
  deriving (Show, Eq)

instance FromEnv Host where
  fromEnv = Host <$> env "HOST"

instance FromEnv Port where
  fromEnv = Port <$> envMaybe "PORT" .!= 1337
