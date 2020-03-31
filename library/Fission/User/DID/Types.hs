-- | 

module Fission.User.DID.Types (Key (..)) where

import           Fission.Prelude
import           Fission.PublicKey.Types

-- _Probably_ toss this whole DID thing for now
newtype Key = Key
  { unKey :: PublicKey
  }
  deriving (Show, Eq)

