{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Internal.MIME
  ( mimeExt
  , lookupExt
  , defaultMimeLookup
  ) where

import RIO
import RIO.Map as Map

import Network.Mime

lookupExt :: MimeType -> Extension
lookupExt mime = fromMaybe "blob" (mimeExt !? mime)

-- NOTE not a one-to-one mapping, so we may get some funky results.
-- Have hand-tested many common types, and am getting the expected extensions.
-- ~BEZ
mimeExt :: Map MimeType Extension
mimeExt = Map.fromList $ swap <$> Map.assocs defaultMimeMap
  where
    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)
