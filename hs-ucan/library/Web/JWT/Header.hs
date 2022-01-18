module Web.JWT.Header
  ( newEd25519
  , module Web.JWT.Header.Types
  ) where

import RIO

import           Crypto.Key.Asymmetric.Algorithm.Types
import           Web.SemVer.Types

import           Web.JWT.Header.Types

import qualified Web.JWT.Header.Typ.Types              as Typ

newEd25519 :: Header
newEd25519 =
  Header
    { typ = Typ.JWT
    , alg = Ed25519
    , cty = Nothing
    , uav = SemVer 0 3 1
    }
