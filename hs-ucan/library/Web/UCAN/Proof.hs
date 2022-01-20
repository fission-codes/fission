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


delegatedInBounds ::
  ( ResourceSemantics rsc
  , ResourceSemantics ptc
  )
  => UCAN fct rsc ptc
  -> UCAN fct rsc ptc
  -> Either Error (UCAN fct rsc ptc)
delegatedInBounds  ucan prfUCAN = do
  signaturesMatch  ucan prfUCAN
  resourceInSubset ucan prfUCAN
  potencyInSubset  ucan prfUCAN
  timeInSubset     ucan prfUCAN

signaturesMatch :: UCAN fct rsc ptc -> UCAN fct rsc ptc -> Either Error (UCAN fct rsc ptc)
signaturesMatch ucan prfUCAN =
  if (ucan & claims & sender) == (prfUCAN & claims & receiver)
    then Right ucan
    else Left InvalidSignatureChain

resourceInSubset :: ResourceSemantics rsc => UCAN fct rsc ptc -> UCAN fct rsc ptc -> Either Error (UCAN fct rsc ptc)
resourceInSubset ucan prfUCAN =
  if (prfUCAN & claims & resource) `canDelegate` (ucan & claims & resource)
    then Right ucan
    else Left ScopeOutOfBounds

potencyInSubset :: ResourceSemantics ptc => UCAN fct rsc ptc -> UCAN fct rsc ptc -> Either Error (UCAN fct rsc ptc)
potencyInSubset ucan prfUCAN =
  if (prfUCAN & claims & potency) `canDelegate` (ucan & claims & potency)
    then Right ucan
    else Left PotencyEscelation

timeInSubset :: UCAN fct rsc ptc -> UCAN fct rsc ptc -> Either Error (UCAN fct rsc ptc)
timeInSubset ucan prfUCAN =
  if startBoundry && expiryBoundry
    then Right ucan
    else Left TimeNotSubset

  where
    startBoundry  = (ucan & claims & nbf) >= (prfUCAN & claims & nbf)
    expiryBoundry = (ucan & claims & exp) <= (prfUCAN & claims & exp)

containsFact :: UCAN fct rsc ptc -> ([fct] -> Either Error ()) -> Either Error (UCAN fct rsc ptc)
containsFact ucan factChecker =
  ucan
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right ucan
