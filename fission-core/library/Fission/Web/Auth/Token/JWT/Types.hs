module Fission.Web.Auth.Token.JWT.Types (FissionJWT, Proof) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Web.JWT.Types                                    as JWT
import Fission.Error.NotFound.Types


type FissionJWT = JWT.JWT Fact (Scope Resource)

type Proof = JWT.Proof Fact (Scope Resource)

instance Display (NotFound FissionJWT) where
  display _ = "Unable to find UCAN"

instance Display (NotFound Resource) where
  display _ = "No UCAN resource provided (closed UCAN)"
