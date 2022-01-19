module Web.Ucan.Proof
  ( delegatedInBounds
  , signaturesMatch
  , resourceInSubset
  , potencyInSubset
  , containsFact

  -- * Reexport

  , module Web.Ucan.Proof.Error
  , module Web.Ucan.Proof.Class
  ) where

import           RIO                  hiding (exp)

import           Web.Ucan.Proof.Class
import           Web.Ucan.Proof.Error
import           Web.Ucan.Types       as Ucan


delegatedInBounds :: ResourceSemantics rsc => Ucan fct rsc -> Ucan fct rsc -> Either Error (Ucan fct rsc)
delegatedInBounds  ucan prfUcan = do
  signaturesMatch  ucan prfUcan
  resourceInSubset ucan prfUcan
  potencyInSubset  ucan prfUcan
  timeInSubset     ucan prfUcan

signaturesMatch :: Ucan fct rsc -> Ucan fct rsc -> Either Error (Ucan fct rsc)
signaturesMatch ucan prfUcan =
  if (ucan & claims & sender) == (prfUcan & claims & receiver)
    then Right ucan
    else Left InvalidSignatureChain

resourceInSubset :: ResourceSemantics rsc => Ucan fct rsc -> Ucan fct rsc -> Either Error (Ucan fct rsc)
resourceInSubset ucan prfUcan =
  if (prfUcan & claims & resource) `canDelegate` (ucan & claims & resource)
    then Right ucan
    else Left ScopeOutOfBounds

potencyInSubset :: Ucan fct rsc -> Ucan fct rsc -> Either Error (Ucan fct rsc)
potencyInSubset ucan prfUcan =
  if (ucan & claims & potency) <= (prfUcan & claims & potency)
    then Right ucan
    else Left PotencyEscelation

timeInSubset :: Ucan fct rsc -> Ucan fct rsc -> Either Error (Ucan fct rsc)
timeInSubset ucan prfUcan =
  if startBoundry && expiryBoundry
    then Right ucan
    else Left TimeNotSubset

  where
    startBoundry  = (ucan & claims & nbf) >= (prfUcan & claims & nbf)
    expiryBoundry = (ucan & claims & exp) <= (prfUcan & claims & exp)

containsFact :: Ucan fct rsc -> ([fct] -> Either Error ()) -> Either Error (Ucan fct rsc)
containsFact ucan factChecker =
  ucan
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right ucan
