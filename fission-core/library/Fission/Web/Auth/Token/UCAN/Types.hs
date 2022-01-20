module Fission.Web.Auth.Token.UCAN.Types (UCAN, Proof) where

import           Fission.Web.Auth.Token.UCAN.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types

import qualified Web.UCAN.Types                                   as UCAN


type UCAN = UCAN.UCAN Fact (Scope Resource) Potency

type Proof = UCAN.Proof Fact (Scope Resource) Potency
