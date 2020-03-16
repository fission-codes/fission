module Fission.App.Domain.Class (HasBaseAppDomain (..)) where

import           Fission.Prelude
import           Fission.URL.DomainName.Types (DomainName)

class Monad m => HasBaseAppDomain m where
  -- | The default domain that's managed by Fission
  get :: m DomainName

instance HasBaseAppDomain m => HasBaseAppDomain (Transaction m) where
  get = lift get
