module Fission.App.Domain.Initializer.Class (Initializer (..)) where

import           Fission.Prelude
import           Fission.URL.DomainName.Types (DomainName)

class Monad m => Initializer m where
  -- | The default domain that's managed by Fission
  initial :: m DomainName

instance Initializer m => Initializer (Transaction m) where
  initial = lift initial
