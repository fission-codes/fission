module Fission.Web.Auth.Token.JWT.Types (FissionJWT, Proof) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Error.NotFound.Types
import qualified Web.Ucan.Types                                   as JWT


type FissionJWT = JWT.JWT Fact (Scope Resource)

type Proof = JWT.Proof Fact (Scope Resource)
