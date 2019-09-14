module Fission.Web.Client.Types (Runner (..)) where

import RIO

import Servant.Client

newtype Runner = Runner
  { getRunner :: forall a. ClientM a -> IO (Either ServantError a) }
