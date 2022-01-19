module Web.UCAN.Proof
  ( delegatedInBounds
  , signaturesMatch
  , resourceInSubset
  , potencyInSubset
  , containsFact

  -- * Reexport

  , module Web.UCAN.Proof.Error
  , module Web.UCAN.Proof.Class
  ) where

import           RIO                  hiding (exp)

import           Web.UCAN.Proof.Class
import           Web.UCAN.Proof.Error
import           Web.UCAN.Types       as UCAN


delegatedInBounds :: ResourceSemantics rsc => UCAN fct rsc -> UCAN fct rsc -> Either Error (UCAN fct rsc)
delegatedInBounds  ucan prfUCAN = do
  signaturesMatch  ucan prfUCAN
  resourceInSubset ucan prfUCAN
  potencyInSubset  ucan prfUCAN
  timeInSubset     ucan prfUCAN

signaturesMatch :: UCAN fct rsc -> UCAN fct rsc -> Either Error (UCAN fct rsc)
signaturesMatch ucan prfUCAN =
  if (ucan & claims & sender) == (prfUCAN & claims & receiver)
    then Right ucan
    else Left InvalidSignatureChain

resourceInSubset :: ResourceSemantics rsc => UCAN fct rsc -> UCAN fct rsc -> Either Error (UCAN fct rsc)
resourceInSubset ucan prfUCAN =
  if (prfUCAN & claims & resource) `canDelegate` (ucan & claims & resource)
    then Right ucan
    else Left ScopeOutOfBounds

potencyInSubset :: UCAN fct rsc -> UCAN fct rsc -> Either Error (UCAN fct rsc)
potencyInSubset ucan prfUCAN =
  if (ucan & claims & potency) <= (prfUCAN & claims & potency)
    then Right ucan
    else Left PotencyEscelation

timeInSubset :: UCAN fct rsc -> UCAN fct rsc -> Either Error (UCAN fct rsc)
timeInSubset ucan prfUCAN =
  if startBoundry && expiryBoundry
    then Right ucan
    else Left TimeNotSubset

  where
    startBoundry  = (ucan & claims & nbf) >= (prfUCAN & claims & nbf)
    expiryBoundry = (ucan & claims & exp) <= (prfUCAN & claims & exp)

containsFact :: UCAN fct rsc -> ([fct] -> Either Error ()) -> Either Error (UCAN fct rsc)
containsFact ucan factChecker =
  ucan
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right ucan
