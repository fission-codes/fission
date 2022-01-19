module Fission.CLI.WNFS.Access.Mutation.Authorization.Types (Authorization (..)) where

import qualified Crypto.PubKey.Ed25519             as Ed25519

import           Fission.Prelude

import           Fission.Web.Auth.Token.Ucan.Types
import           Web.Ucan.RawContent.Types         as Ucan

data Authorization
  = Root Ed25519.SecretKey
  | Delegated Ucan Ucan.RawContent
  deriving (Show, Eq)
