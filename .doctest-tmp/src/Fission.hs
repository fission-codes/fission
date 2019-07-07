{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission (fromConfig) where

import RIO

import Data.Has

-- | Get a value from the reader config
--
-- >>> newtype Example = Example Text deriving Show
-- >>>
-- >>> data ExCfg = ExCfg { example :: Text }
-- >>> instance Has ExCfg Example where hasLens = example
-- >>>
-- >>> runRIO (ExCfg "hello world") (fromConfig :: Example)
-- Example "hello world"
fromConfig :: (MonadReader cfg m, Has a cfg) => m a
fromConfig = view hasLens
