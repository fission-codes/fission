{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Platform.Heroku.UserConfig
  ( UserConfig (..)
  , interplanetaryFissionUrl
  , interplanetaryFissionPassword
  , interplanetaryFissionUsername
  ) where

import RIO

import Control.Lens
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Swagger as Swagger

import Fission.Internal.JSON
import Fission.Security

data UserConfig = UserConfig
  { _interplanetaryFissionUrl      :: Text
  , _interplanetaryFissionUsername :: Text
  , _interplanetaryFissionPassword :: Secret
  } deriving ( Eq
             , Show
             , Generic
             )

makeLenses ''UserConfig
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''UserConfig)

instance ToSchema UserConfig where
  declareNamedSchema = genericDeclareNamedSchema
    $ (fromAesonOptions lens_SCREAMING_SNAKE_CASE)
      { Swagger.constructorTagModifier = camelCase }
