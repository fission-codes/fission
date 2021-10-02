module Fission.Web.Purge.Types (PURGE (..)) where

import           Network.HTTP.Req

import           Fission.Prelude

data PURGE = PURGE
  deriving (Show, Eq)

instance HttpMethod PURGE where
  type AllowsBody _ = 'NoBody
  httpMethodName _pxy = "PURGE"
