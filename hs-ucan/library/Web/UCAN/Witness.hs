module Web.UCAN.Witness
  ( delegatedInBounds
  , signaturesMatch
  , containsFact

  -- * Reexport

  , module Web.UCAN.Witness.Error
  , module Web.UCAN.Witness.Class
  ) where

import           RIO                    hiding (exp)

import           Web.UCAN.Types         as UCAN
import           Web.UCAN.Witness.Class
import           Web.UCAN.Witness.Error


delegatedInBounds ::
  UCAN fct res abl
  -> UCAN fct res abl
  -> Either Error (UCAN fct res abl)
delegatedInBounds ucan witness = do
  signaturesMatch ucan witness

signaturesMatch :: UCAN fct res abl -> UCAN fct res abl -> Either Error (UCAN fct res abl)
signaturesMatch ucan witness =
  if (ucan & claims & sender) == (witness & claims & receiver)
    then Right ucan
    else Left InvalidSignatureChain

containsFact :: UCAN fct res abl -> ([fct] -> Either Error ()) -> Either Error (UCAN fct res abl)
containsFact ucan factChecker =
  ucan
    & claims
    & facts
    & factChecker
    & \case
        Left err -> Left err
        Right () -> Right ucan
