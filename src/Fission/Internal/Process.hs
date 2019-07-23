module Fission.Internal.Process
  ( RIOProc
  , StreamIn
  , StreamOut
  ) where

import RIO
import RIO.Process

import Fission.Internal.Constraint
import Fission.Config.Types ()

type RIOProc cfg m = ( MonadRIO          cfg m
                     , HasProcessContext cfg
                     , HasLogFunc        cfg
                     )

type StreamIn  stdin  = StreamSpec 'STInput  stdin
type StreamOut stdout = StreamSpec 'STOutput stdout
