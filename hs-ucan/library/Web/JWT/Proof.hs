module Web.JWT.Proof
  ( delegatedInBounds
  , signaturesMatch
  , resourceInSubset
  , potencyInSubset
  , containsFact

  -- * Reexport

  , module Web.JWT.Proof.Error
  , module Web.JWT.Proof.Class
  ) where

import           RIO hiding (exp)

import           Web.JWT.Proof.Class
import           Web.JWT.Proof.Error
import           Web.JWT.Types                                    as JWT


delegatedInBounds :: ResourceSemantics rsc => JWT fct rsc -> JWT fct rsc -> Either Error (JWT fct rsc)
delegatedInBounds  jwt prfJWT = do
  signaturesMatch  jwt prfJWT
  resourceInSubset jwt prfJWT
  potencyInSubset  jwt prfJWT
  timeInSubset     jwt prfJWT

signaturesMatch :: JWT fct rsc -> JWT fct rsc -> Either Error (JWT fct rsc)
signaturesMatch jwt prfJWT =
  if (jwt & claims & sender) == (prfJWT & claims & receiver)
    then Right jwt
    else Left InvalidSignatureChain

resourceInSubset :: ResourceSemantics rsc => JWT fct rsc -> JWT fct rsc -> Either Error (JWT fct rsc)
resourceInSubset jwt prfJWT =
  if (prfJWT & claims & resource) `canDelegate` (jwt & claims & resource)
    then Right jwt
    else Left ScopeOutOfBounds

potencyInSubset :: JWT fct rsc -> JWT fct rsc -> Either Error (JWT fct rsc)
potencyInSubset jwt prfJWT =
  if (jwt & claims & potency) <= (prfJWT & claims & potency)
    then Right jwt
    else Left PotencyEscelation

timeInSubset :: JWT fct rsc -> JWT fct rsc -> Either Error (JWT fct rsc)
timeInSubset jwt prfJWT =
  if startBoundry && expiryBoundry
    then Right jwt
    else Left TimeNotSubset

  where
    startBoundry  = (jwt & claims & nbf) >= (prfJWT & claims & nbf)
    expiryBoundry = (jwt & claims & exp) <= (prfJWT & claims & exp)

containsFact :: JWT fct rsc -> ([fct] -> Either Error ()) -> Either Error (JWT fct rsc)
containsFact jwt factChecker =
  jwt
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right jwt
