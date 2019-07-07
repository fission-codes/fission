{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.IPFS.Peer
  ( all
  , rawList
  ) where

import           RIO hiding (all)
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text
import           RIO.Process (HasProcessContext)

import Data.Has

import           Fission.Internal.Constraint
import qualified Fission.IPFS.Process        as IPFSProc
import qualified Fission.IPFS.Types          as IPFS
import qualified Fission.Internal.UTF8       as UTF8

all :: MonadRIO cfg m
    => HasProcessContext cfg
    => HasLogFunc cfg
    => Has IPFS.BinPath cfg
    => m (Either UnicodeException [IPFS.Peer])
all = do
  allRaw <- rawList

  let
    textOrErr      = UTF8.encode allRaw
    peerNamesOrErr = Text.lines <$> textOrErr

  return $ fmap IPFS.Peer <$> peerNamesOrErr

rawList :: MonadRIO          cfg m
        => Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => m Lazy.ByteString
rawList = IPFSProc.run' ["bootstrap", "list"]
