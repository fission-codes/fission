{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Plan (Tier (..)) where

import RIO
import RIO.Text as Text

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Casing
import Data.Swagger as Swagger

data Tier
  = Test
  | Free
  --  | Paid
  deriving ( Eq
           , Show
           , Generic
           )

makeLenses ''Tier

instance ToJSON Tier where
  toJSON = String . Text.toLower . textDisplay . displayShow

instance FromJSON Tier where
  parseJSON (String str) =
    case str of
      "Test" -> pure Test
      "test" -> pure Test

      "Free" -> pure Free
      "free" -> pure Free

      other  -> cantParse other

  parseJSON other = cantParse other

instance ToSchema Tier where
  declareNamedSchema = genericDeclareNamedSchema
    $ defaultSchemaOptions
      { Swagger.constructorTagModifier = camelCase }

cantParse :: (Monad m, Show a) => a -> m b
cantParse other = fail $ "Unable to parse " <> show other
