{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Web.IPFS.Upload.Multipart
  ( API
  , add
  , jsonAdd
  , textAdd
  ) where

import RIO
import RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Data.Has
import Servant
import Servant.Multipart

import           Fission.Internal.Constraint
import           Fission.Internal.MIME

import           Fission.Web.Server
import qualified Fission.IPFS.SparseTree as IPFS
import qualified Fission.IPFS.Types      as IPFS
import qualified Fission.Storage.IPFS    as Storage.IPFS
import qualified Fission.Web.Error       as Web.Err

type API = TextAPI :<|> JSONAPI

type TextAPI = FileRequest
               :> NameQuery
               :> Post '[OctetStream, PlainText] IPFS.Path

type JSONAPI = FileRequest
               :> NameQuery
               :> Post '[JSON] IPFS.SparseTree

type FileRequest = MultipartForm Mem (MultipartData Mem)
type NameQuery   = QueryParam "name" IPFS.Name

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
add = textAdd :<|> jsonAdd

textAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg TextAPI
textAdd form queryName = run form queryName $ \sparse ->
  case IPFS.linearize sparse of
    Right x   -> pure x
    Left  err -> Web.Err.throw err

jsonAdd :: Has IPFS.BinPath  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg JSONAPI
jsonAdd form queryName = run form queryName pure

run :: MonadRIO          cfg m
    => MonadThrow            m
    => Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => MultipartData Mem
    -> Maybe IPFS.Name
    -> (IPFS.SparseTree -> m a)
    -> m a
run form qName cont = case lookupFile "file" form of
  Nothing -> throwM $ err422 { errBody = "File not processable by IPFS" }
  Just FileData { .. } ->
    Storage.IPFS.addFile fdPayload humanName >>= \case
      Right struct -> cont struct
      Left  err    -> Web.Err.throw err
    where
      humanName :: IPFS.Name
      humanName = name qName fdFileName fdFileCType

name :: Maybe IPFS.Name -> Text -> Text -> IPFS.Name
name queryName' fileName mime =
  case queryName' of
    Nothing              -> IPFS.Name $ plainName fileName mime
    Just (IPFS.Name "")  -> IPFS.Name $ plainName fileName mime
    Just ipfsName        -> ipfsName

plainName :: Text -> Text -> String
plainName ""       mime = Text.unpack $ "file." <> lookupExt (encodeUtf8 mime)
plainName fileName _    = Text.unpack fileName
