-- | A custom @Prelude@-like module for this project
module Fission.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Data.Has
  , module Data.Maybe
  , module Fission.Internal.Constraint
  , module Fission.Internal.List
  , module Flow
  , module RIO
  , module RIO.Process
  , module RIO.Time
  , bind
  , headMaybe
  , identity
  , intercalate
  , putText
  , putTextLn
  , textShow
  ) where

import Control.Lens                ((%~), (.~), (?~), (^?))
import Data.Aeson
import Data.Has
import Data.Maybe
import Fission.Internal.Constraint
import Fission.Internal.List
import Fission.Internal.UTF8       (putText, putTextLn, textShow)
import Flow
import RIO                         hiding (Handler, id, timeout, ($), (&))
import RIO.List                    (headMaybe, intercalate)
import RIO.Process
import RIO.Time

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)

identity :: a -> a
identity a = a
