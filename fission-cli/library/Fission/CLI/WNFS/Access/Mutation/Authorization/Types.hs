module Fission.CLI.WNFS.Access.Mutation.Authorization.Types (Authorization (..)) where

import qualified Crypto.PubKey.Ed25519             as Ed25519

import           Fission.Prelude

import           Fission.Web.Auth.Token.UCAN.Types
import           Web.UCAN.RawContent.Types         as UCAN

data Authorization
  = Root Ed25519.SecretKey
  | Delegated UCAN UCAN.RawContent
  deriving (Show, Eq)
