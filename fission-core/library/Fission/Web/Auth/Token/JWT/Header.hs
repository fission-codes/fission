module Fission.Web.Auth.Token.JWT.Header
  ( newEd25519
  , module Fission.Web.Auth.Token.JWT.Header.Types
  ) where

import           Fission.Prelude

import           Fission.Key.Asymmetric.Algorithm.Types
import           Fission.SemVer.Types

import           Fission.Web.Auth.Token.JWT.Header.Types

import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types as Typ

newEd25519 :: Header
newEd25519 =
  Header
    { typ = Typ.JWT
    , alg = Ed25519
    , cty = Nothing
    , uav = SemVer 0 3 1
    }
