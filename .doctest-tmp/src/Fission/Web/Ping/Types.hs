{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Web.Ping.Types (Pong (..)) where

import RIO
import Servant

import Data.Aeson
import Data.Swagger hiding (name)
import Control.Lens

import qualified Fission.Internal.UTF8 as UTF8

-- | A dead-simple text wrapper.
--   Primarily exists for customized instances.
newtype Pong = Pong { unPong :: Text }
  deriving         ( Eq
                   , Show
                   )
  deriving newtype ( IsString
                   , FromJSON
                   , ToJSON
                   )

instance ToSchema Pong where
  declareNamedSchema _ =
     return $ NamedSchema (Just "Pong") $ mempty
            & type_       .~ SwaggerString
            & description ?~ "A simple response"
            & example     ?~ toJSON (Pong "pong")

instance MimeRender PlainText Pong where
  mimeRender _proxy = UTF8.textToLazyBS . unPong
