{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Fission.Security
  ( Secret (..)
  , SecretDigest
  , mkSecret
  , toSecret
  , Digestable
  , digest
  ) where

import RIO

import           Control.Lens
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Random as BS
import           Data.Swagger

import qualified Fission.Internal.UTF8  as UTF8

type SecretDigest = Text

newtype Secret = Secret { unSecret :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  ( FromJSON
                    , ToJSON
                    )

instance ToSchema Secret where
  declareNamedSchema _ =
     return $ NamedSchema (Just "Secret") $ mempty
            & type_   .~ SwaggerString
            & example ?~ "U)mRvIvI6$L_MkYpme!lfzMte_92M5G912-NUfRmfxhRKx$Rr6aLUxqdqW"

mkSecret :: Natural -> IO (Either UnicodeException Secret)
mkSecret = pure . toSecret <=< BS.random

toSecret :: ByteString -> Either UnicodeException Secret
toSecret raw = Secret <$> UTF8.encode raw

class Digestable a where
  digest :: a -> SecretDigest

instance Digestable ByteString where
  digest bs = UTF8.textShow (hash bs :: Digest SHA3_512)

instance Digestable Text where
  digest = digest . encodeUtf8
