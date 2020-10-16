module Fission.Web.Auth.Token.JWT.Proof
  ( delegatedInBounds
  , signaturesMatch
  , resourceInSubset
  , potencyInSubset

  -- * Reexport

  , module Fission.Web.Auth.Token.JWT.Proof.Error
  ) where

import qualified RIO.List                                         as List

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Proof.Error

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

delegatedInBounds :: UCAN -> UCAN -> Either Error UCAN
delegatedInBounds  jwt prfJWT = do
  signaturesMatch  jwt prfJWT
  resourceInSubset jwt prfJWT
  potencyInSubset  jwt prfJWT
  timeInSubset     jwt prfJWT

signaturesMatch :: UCAN -> UCAN -> Either Error UCAN
signaturesMatch jwt prfJWT =
  if (jwt |> claims |> sender) == (prfJWT |> claims |> receiver)
    then Right jwt
    else Left InvalidSignatureChain

resourceInSubset :: UCAN -> UCAN -> Either Error UCAN
resourceInSubset jwt prfJWT =
  case ((jwt |> claims |> resource), (prfJWT |> claims |> resource)) of
    (Subset (FissionFileSystem path), Subset (FissionFileSystem proofPath)) ->
      if path `List.isPrefixOf` proofPath -- NOTE `List` because FilePath ~ String
        then Right jwt
        else Left ScopeOutOfBounds

    (a, b) ->
      if a == b
        then Right jwt
        else Left ScopeOutOfBounds

potencyInSubset :: UCAN -> UCAN -> Either Error UCAN
potencyInSubset jwt prfJWT =
  if (jwt |> claims |> potency) <= (prfJWT |> claims |> potency)
    then Right jwt
    else Left PotencyEscelation

timeInSubset :: UCAN -> UCAN -> Either Error UCAN
timeInSubset jwt prfJWT =
  if startBoundry && expiryBoundry
    then Right jwt
    else Left TimeNotSubset

  where
    startBoundry  = (jwt |> claims |> nbf) >= (prfJWT |> claims |> nbf)
    expiryBoundry = (jwt |> claims |> exp) <= (prfJWT |> claims |> exp)
