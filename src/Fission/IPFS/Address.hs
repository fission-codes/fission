module Fission.IPFS.Address
  ( Address ()
  , mkAddress
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import qualified Data.ByteString.Builder as Builder
import           Servant

import qualified Fission.Internal.UTF8 as UTF8
import           Fission.IPFS.Types

mkAddress :: Lazy.ByteString -> Address
mkAddress = Address . UTF8.stripNewline
