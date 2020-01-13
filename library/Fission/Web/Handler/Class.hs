module Fission.Web.Handler.Class (AsHandler (..)) where

import Servant

class AsHandler m where
  asHandler :: m a -> Handler a
