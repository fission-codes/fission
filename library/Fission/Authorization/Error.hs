module Fission.Authorization.Error (Unauthorized (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

data Unauthorized = Unauthorized
  deriving (Show, Eq)

instance ToServerError Unauthorized where
  toServerError _ = err401
