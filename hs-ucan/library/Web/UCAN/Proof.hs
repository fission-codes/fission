module Web.UCAN.Proof
  ( delegatedInBounds
  , signaturesMatch
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
  DelegationSemantics cap
  => UCAN fct cap
  -> UCAN fct cap
  -> Either Error (UCAN fct cap)
delegatedInBounds  ucan prfUCAN = do
  signaturesMatch  ucan prfUCAN

signaturesMatch :: UCAN fct cap -> UCAN fct cap -> Either Error (UCAN fct cap)
signaturesMatch ucan prfUCAN =
  if (ucan & claims & sender) == (prfUCAN & claims & receiver)
    then Right ucan
    else Left InvalidSignatureChain

containsFact :: UCAN fct cap -> ([fct] -> Either Error ()) -> Either Error (UCAN fct cap)
containsFact ucan factChecker =
  ucan
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right ucan
