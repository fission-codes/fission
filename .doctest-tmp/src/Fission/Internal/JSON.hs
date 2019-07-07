{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Internal.JSON
  ( lens_snake_case
  , lens_SCREAMING_SNAKE_CASE
  , snake_case
  , _SCREAMING_SNAKE_CASE
  ) where

import RIO
import RIO.Char (toUpper)

import Data.Aeson
import Data.Aeson.Casing

-- | Format JSON as `snake_case`.
--   N.B. This does not remove leading underscore from the lens convention
snake_case :: Options
snake_case = defaultOptions { fieldLabelModifier = snakeCase }

-- | Format JSON as `snake_case`
--   Handles the underscore prefix from the generator convention
--   by simply dropping the first character
lens_snake_case :: Options
lens_snake_case = defaultOptions { fieldLabelModifier = drop 1 . snakeCase }

-- | Format JSON as `SCREAMING_SNAKE_CASE`.
--   N.B. This does not remove leading underscore from the lens convention
_SCREAMING_SNAKE_CASE :: Options
_SCREAMING_SNAKE_CASE = defaultOptions
  { fieldLabelModifier = toScreamingSnake }

-- | Format JSON as `SCREAMING_SNAKE_CASE`.
--   Handles the underscore prefix from the generator convention
--   by simply dropping the first character
lens_SCREAMING_SNAKE_CASE :: Options
lens_SCREAMING_SNAKE_CASE = defaultOptions
  { fieldLabelModifier = drop 1 . toScreamingSnake }

toScreamingSnake :: String -> String
toScreamingSnake = fmap toUpper . snakeCase
