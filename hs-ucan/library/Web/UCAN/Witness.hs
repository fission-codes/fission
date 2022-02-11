module Web.UCAN.Witness
  ( delegatedInBounds
  , signaturesMatch
  , containsFact

  -- * Reexport

  , module Web.UCAN.Witness.Error
  , module Web.UCAN.Witness.Class
  ) where

import           RIO                  hiding (exp)

import           Web.UCAN.Witness.Class
import           Web.UCAN.Witness.Error
import           Web.UCAN.Types       as UCAN


delegatedInBounds ::
  DelegationSemantics cap
  => UCAN fct cap
  -> UCAN fct cap
  -> Either Error (UCAN fct cap)
delegatedInBounds ucan witness = do
  signaturesMatch ucan witness

signaturesMatch :: UCAN fct cap -> UCAN fct cap -> Either Error (UCAN fct cap)
signaturesMatch ucan witness =
  if (ucan & claims & sender) == (witness & claims & receiver)
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
