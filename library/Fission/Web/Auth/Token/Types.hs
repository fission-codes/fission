module Fission.Web.Auth.Token.Types (Token (..)) where

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Basic.Types  as Basic
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

data Token
  = Basic  Basic.Token
  | Bearer Bearer.Token
  deriving (Show, Eq)
