-- | A custom @Prelude@-like module for this project
module Fission.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Data.Has
  , module Data.Maybe
  , module Fission.Internal.Constraint
  , module Flow
  , module RIO
  , module RIO.Process
  , NominalDiffTime
  , UTCTime (..)
  , getCurrentTime
  , headMaybe
  , identity
  , intercalate
  ) where

import Control.Lens                ((%~), (.~), (?~), (^?))
import Data.Aeson
import Data.Has
import Data.Maybe
import Data.Time                   (getCurrentTime)
import Fission.Internal.Constraint
import Flow
import RIO                         hiding (Handler, id, timeout, ($), (&))
import RIO.List                    (headMaybe, intercalate)
import RIO.Process
import RIO.Time                    (NominalDiffTime, UTCTime (..))

identity :: a -> a
identity a = a
