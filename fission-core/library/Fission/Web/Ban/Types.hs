module Fission.Web.Ban.Types (BAN (..)) where

import           Network.HTTP.Req

import           Fission.Prelude

data BAN = BAN
  deriving (Show, Eq)

instance HttpMethod BAN where
  type AllowsBody _ = 'NoBody
  httpMethodName _pxy = "BAN"
