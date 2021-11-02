module Network.IPFS.Process.Types
  ( Opt
  , Command
  , StreamIn
  , StreamOut
  , RawMessage
  ) where

import Network.IPFS.Prelude
import Data.ByteString.Lazy.Char8 as CL

type Opt = String
type Command = String
type StreamIn  stdin  = StreamSpec 'STInput  stdin
type StreamOut stdout = StreamSpec 'STOutput stdout
type RawMessage = CL.ByteString
