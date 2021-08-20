module Crypto.Hash.SpiralRatchet.Types (SpiralRatchet (..)) where

import           Crypto.Hash
import           RIO

-- data Dimension

data SpiralRatchet = SpiralRatchet
  { large     :: Digest SHA256

  , medium    :: Digest SHA256
  , mediumMax :: Digest SHA256

  , small     :: Digest SHA256
  , smallMax  :: Digest SHA256
  }
  deriving (Show, Eq, Ord)
