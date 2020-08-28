module Fission.App.Content.Initializer.Class (Initializer (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude

class Monad m => Initializer m where
  -- | Placeholder app content (e.g. splash page)
  placeholder :: m CID

instance Initializer m => Initializer (Transaction m) where
  placeholder = lift placeholder
