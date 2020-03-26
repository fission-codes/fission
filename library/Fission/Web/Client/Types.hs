module Fission.Web.Client.Types (Runner (..)) where

import Fission.Prelude

import Servant.Client

newtype Runner = Runner
  { getRunner :: forall a. ClientM a -> IO (Either ClientError a) }
