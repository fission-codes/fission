module Fission.Internal.Process
  ( RIOProc
  , StreamIn
  , StreamOut
  ) where

import RIO
import RIO.Process

import Data.Has

import Fission.Internal.Constraint
import Fission.Config.Types ()

type RIOProc cfg m = ( MonadRIO           cfg m
                     , Has ProcessContext cfg
                     , Has LogFunc         cfg
                     )

type StreamIn  stdin  = StreamSpec 'STInput  stdin
type StreamOut stdout = StreamSpec 'STOutput stdout
