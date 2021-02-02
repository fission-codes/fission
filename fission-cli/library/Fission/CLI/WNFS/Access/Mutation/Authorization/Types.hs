-- FIXME can be moved
module Fission.CLI.WNFS.Access.Mutation.Authorization.Types (Authorization (..)) where

import qualified Crypto.PubKey.Ed25519      as Ed25519

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.JWT as UCAN

data Authorization
  = Root Ed25519.SecretKey
  | Delegated UCAN.JWT UCAN.RawContent
  deriving (Show, Eq)
