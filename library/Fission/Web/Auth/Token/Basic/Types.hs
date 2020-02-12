-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Basic.Types (Token (..)) where

import Fission.Prelude

newtype Token = Token { unToken :: ByteString }
  deriving (Show, Eq)
