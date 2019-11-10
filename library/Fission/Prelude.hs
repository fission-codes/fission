-- | A custom @Prelude@-like module for this project
module Fission.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Data.Has
  , module Data.Maybe
  , module Flow
  , module RIO
  , module RIO.Process
  , identity
  , NominalDiffTime
  , UTCTime (..)
  ) where

import Control.Lens ((%~), (.~), (?~), (^?))
import Data.Aeson
import Data.Has
import Data.Maybe
import Flow
import RIO          hiding (id, timeout, ($), (&))
import RIO.Process
import RIO.Time     (NominalDiffTime, UTCTime (..))

identity :: a -> a
identity a = a
