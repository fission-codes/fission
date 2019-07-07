{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.IPFS.Error
  ( Add (..)
  , Error (..)
  , Linearization (..)
  ) where

import RIO

import Data.Aeson
import Network.HTTP.Types.Status
import Servant.Exception

import Fission.IPFS.Types

data Error
  = AddErr Add
  | LinearizationErr Linearization
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance ToServantErr Error where
  status (AddErr           addErr) = status addErr
  status (LinearizationErr linErr) = status linErr

  message (AddErr           addErr) = message addErr
  message (LinearizationErr linErr) = message linErr

data Add
  = InvalidFile
  | UnexpectedOutput Text
  | UnknownError
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Add where
  display (UnexpectedOutput txt) = "Unexpected IPFS output: " <> display txt
  display err                    = displayShow err

instance ToServantErr Add where
  status = \case
    InvalidFile        -> status422
    UnknownError       -> internalServerError500
    UnexpectedOutput _ -> internalServerError500

  message = \case
    InvalidFile        -> "File not processable by IPFS"
    UnknownError       -> "Unknown IPFS error"
    UnexpectedOutput _ -> "Unexpected IPFS result"

data Linearization
  = NonLinear SparseTree
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Linearization where
  display (NonLinear sparseTree) = "Unable to linearize IPFS result: " <> display sparseTree

instance ToServantErr Linearization where
  status  _ = internalServerError500
  message _ = "Unable to linearize IPFS result"
