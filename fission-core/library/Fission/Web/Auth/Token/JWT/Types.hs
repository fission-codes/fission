module Fission.Web.Auth.Token.JWT.Types (JWT(..), Proof) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import Fission.Error.NotFound.Types

import qualified Web.JWT.Types                                    as JWT


newtype JWT = FissionJWT { unFissionJWT :: JWT.JWT Fact (Scope Resource) }
  deriving newtype (Display, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Eq)

type Proof = JWT.Proof Fact (Scope Resource)

instance Display (NotFound JWT) where
  display _ = "Unable to find UCAN"
