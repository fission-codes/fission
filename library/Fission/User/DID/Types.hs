-- | 

module Fission.User.DID.Types
  ( DID    (..)
  , Method (..)
  , module Fission.PublicKey.Types
  ) where

import           Fission.Prelude
import           Fission.PublicKey.Types

data DID = DID
  { publicKey :: !PublicKey
  , algorithm :: !Algorithm
  , method    :: !Method
  } deriving (Show, Eq)

-- instance ToJSON Key where
--   toJSON =

data Method
  = Key
  deriving (Show, Eq)
