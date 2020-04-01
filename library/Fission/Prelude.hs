-- | A custom @Prelude@-like module for this project
module Fission.Prelude
  ( module Control.Lens
  , module Control.Monad.Logger
  , module Control.Monad.Time
  , module Data.Aeson
  , module Data.Bifunctor
  , module Data.Has
  , module Data.Maybe
  , module Data.WorldPeace
  , module Fission.Internal.Log
  , module Fission.Internal.MonadDB
  , module Flow
  , module RIO
  , module RIO.Process
  , module RIO.Time
  , module Test.QuickCheck
  , module Web.PathPieces
  , Entity (..)
  , headMaybe
  , identity
  , intercalate
  , putText
  , putTextLn
  , textShow
  , displayLazyBS
  , bind
  , ok
  , noop
  ) where

import Control.Lens         ((%~), (.~), (?~), (^?))
import Control.Monad.Logger (MonadLogger (..), LogSource, ToLogStr (..), LogLevel (..), logWithoutLoc)
import Control.Monad.Time

import Network.IPFS.Internal.Orphanage.Utf8Builder ()

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Has
import Data.Maybe
import Data.WorldPeace

import Database.Persist (Entity (..))

import Flow

import RIO.List (headMaybe, intercalate)
import RIO.Process
import RIO.Time

import RIO.Orphans ()

import RIO hiding ( Handler
                  , LogLevel (..)
                  , LogSource
                  , logDebug
                  , logDebugS
                  , logError
                  , logErrorS
                  , logInfo
                  , logInfoS
                  , logOther
                  , logOtherS
                  , logWarn
                  , logWarnS
                  , id
                  , timeout
                  , const
                  , (&)
                  , (^.)
                  , exp
                  )

import Test.QuickCheck hiding (Result (..))
import Test.QuickCheck.Instances ()

import Web.PathPieces

import Fission.Internal.Orphanage.OpenUnion ()
import Fission.Internal.MonadDB
import Fission.Internal.Log
import Fission.Internal.UTF8       (putText, putTextLn, textShow, displayLazyBS)

identity :: a -> a
identity a = a

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)

ok :: Either err ()
ok = Right ()

noop :: Applicative f => f ()
noop = pure ()
