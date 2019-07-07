{-# LANGUAGE ApplicativeDo, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, OverloadedLabels, OverloadedLists, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, TemplateHaskell, TypeOperators, ViewPatterns #-}
module Fission.Random
  ( byteString
  , text
  ) where

import           RIO
import qualified RIO.ByteString as BS

import qualified Data.ByteString.Random as BS
import           Data.Word8

import Fission.Internal.Bool (anyX)

text :: Natural -> IO Text
text amount = decodeUtf8Lenient <$> byteString amount

byteString :: Natural -> IO ByteString
byteString amount = BS.filter isURL <$> BS.random amount

isURL :: Word8 -> Bool
isURL w = isAsciiUpper w
        || isAsciiLower w
        || isDigit w
        || anyX urlSpecials w

urlSpecials :: [Word8 -> Bool]
urlSpecials =
  fmap (==)
    [ _asterisk
    , _comma
    , _dollar
    , _exclam
    , _hyphen
    , _parenleft
    , _parenright
    , _period
    , _plus
    , _underscore
    ]
