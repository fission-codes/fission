{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Storage.IPFS
  ( addRaw
  , addFile
  , pin
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has
import Data.ByteString.Lazy.Char8 as BS

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8       as UTF8
import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import qualified Fission.IPFS.Process        as IPFS.Proc

addRaw :: MonadRIO          cfg m
       => HasProcessContext cfg
       => HasLogFunc        cfg
       => Has IPFS.BinPath  cfg
       => Lazy.ByteString
       -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw = BS.lines <$> IPFS.Proc.run ["add", "-q"] raw >>= pure . \case
  [cid] -> Right . mkCID $ UTF8.textShow cid
  bad   -> Left . UnexpectedOutput $ UTF8.textShow bad

addFile :: MonadRIO          cfg m
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => Has IPFS.BinPath  cfg
        => Lazy.ByteString
        -> IPFS.Name
        -> m (Either IPFS.Error.Add IPFS.SparseTree)
addFile raw name = BS.lines <$> IPFS.Proc.run opts raw >>= pure . \case
  [inner, outer] ->
    let
      sparseTree  = Directory [(Hash rootCID, fileWrapper)]
      fileWrapper = Directory [(fileName, Content fileCID)]
      rootCID     = CID $ UTF8.textShow outer
      fileCID     = CID $ UTF8.textShow inner
      fileName    = Key name
    in
      Right sparseTree

  bad ->
    Left . UnexpectedOutput $ UTF8.textShow bad

  where
    opts = [ "add"
           , "-wq"
           , "--stdin-name"
           , unName name
           ]

pin :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.BinPath  cfg
    => IPFS.CID
    -> m (Either IPFS.Error.Add ())
pin (CID cid) = IPFS.Proc.run_ ["pin", "add"] (UTF8.textToLazyBS cid) >>= pure . \case
  ExitSuccess   -> Right ()
  ExitFailure _ -> Left UnknownError
