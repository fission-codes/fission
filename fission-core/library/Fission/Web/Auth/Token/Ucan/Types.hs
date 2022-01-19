module Fission.Web.Auth.Token.Ucan.Types (Ucan, Proof) where

import           Fission.Web.Auth.Token.Ucan.Fact.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Scope.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Types

import qualified Web.Ucan.Types                                   as Ucan


type Ucan = Ucan.Ucan Fact (Scope Resource)

type Proof = Ucan.Proof Fact (Scope Resource)
