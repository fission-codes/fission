module Web.UCAN.Header
  ( newEd25519
  , module Web.UCAN.Header.Types
  ) where

import           Crypto.Key.Asymmetric.Algorithm.Types

import           Web.UCAN.Header.Types

import qualified Web.UCAN.Header.Typ.Types             as Typ

newEd25519 :: Header
newEd25519 =
  Header
    { typ = Typ.JWT
    , alg = Ed25519
    , ucv = ucanVersion
    }
