{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Platform.Heroku.AddOn.Manifest
  ( Manifest (..)
  , id
  , name
  , api
  , API (..)
  , password
  , ssoSalt
  ) where

import RIO hiding (id)

import Control.Lens (makeLenses)
import Data.Aeson.TH

import Fission.Internal.JSON

data API = API
  { _password :: Text
  , _ssoSalt  :: Text
  } deriving ( Show
             , Eq
             )

makeLenses ''API
$(deriveJSON lens_snake_case ''API)

data Manifest = Manifest
  { _id   :: Text
  , _name :: Text
  , _api  :: API
  } deriving ( Show
             , Eq
             )

makeLenses ''Manifest
$(deriveJSON lens_snake_case ''Manifest)
