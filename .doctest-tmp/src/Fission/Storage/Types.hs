{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Storage.Types
  ( Path (..)
  , Pool (..)
  , SeldaPool
  ) where

import RIO

import qualified Data.Pool              as Database
import           Data.Swagger           (ToSchema)
import           Database.Selda.Backend (SeldaConnection)
import           System.Envy

type SeldaPool = Database.Pool SeldaConnection

newtype Pool = Pool { getPool :: SeldaPool }
  deriving Show

newtype Path = Path { getPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromEnv Path where
  fromEnv = Path <$> env "DB_PATH"
