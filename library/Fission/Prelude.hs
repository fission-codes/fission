-- | A custom @Prelude@-like module for this project
module Fission.Prelude
  ( module Control.Lens
  , module Control.Monad.Logger
  , module Control.Monad.Time
  , module Data.Aeson
  , module Data.Has
  , module Data.Maybe
  , module Fission.Internal.Log
  , module Fission.Internal.MonadDB
  , module Flow
  , module RIO
  , module RIO.Process
  , module RIO.Time
  , headMaybe
  , identity
  , intercalate
  , putText
  , putTextLn
  , textShow
  , bind
  ) where

import Control.Lens         ((%~), (.~), (?~), (^?))
import Control.Monad.Logger (MonadLogger (..), LogSource, ToLogStr (..), LogLevel (..), logWithoutLoc)
import Control.Monad.Time

import Network.IPFS.Internal.Orphanage.Utf8Builder ()

import Data.Aeson
import Data.Has
import Data.Maybe

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
                  , ($)
                  , (&)
                  , (^.)
                  )

import Fission.Internal.MonadDB
import Fission.Internal.Log
import Fission.Internal.UTF8       (putText, putTextLn, textShow)

identity :: a -> a
identity a = a

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)
