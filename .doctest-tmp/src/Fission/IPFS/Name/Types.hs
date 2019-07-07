{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.IPFS.Name.Types (Name (..)) where

import           RIO
import qualified RIO.Text    as Text

import Data.Aeson
import Servant
import Data.Swagger (ToParamSchema, ToSchema (..))

newtype Name = Name { unName :: String }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving anyclass ( ToParamSchema
                    , ToSchema
                    )
  deriving newtype  ( IsString )

instance Display Name where
  display = displayShow

instance ToJSON Name where
  toJSON (Name n) = toJSON n

instance FromJSON Name where
  parseJSON = withText "IPFSName" (pure . Name . Text.unpack)

instance FromHttpApiData Name where
  parseUrlPiece = \case
    ""  -> Left "Empty Name field"
    txt -> Right . Name $ Text.unpack txt
