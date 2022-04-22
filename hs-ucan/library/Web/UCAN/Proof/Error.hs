module Web.UCAN.Proof.Error (Error (..)) where

import           RIO
import           RIO.Time
import           Web.DID.Types
import           Web.SemVer.Types


data Error
  = IssuerAudienceMismatch DID DID
  | NotBeforeProofExpired UTCTime UTCTime
  | ExpiresAfterNotBefore UTCTime UTCTime
  | DecreasingVersionInChain SemVer SemVer
  deriving (Show, Eq, Exception)


instance Display Error where
  display = \case
    IssuerAudienceMismatch iss aud -> "Issuer and audience mismatch: issuer " <> display iss <> " mismatches audience " <> display aud
    NotBeforeProofExpired _ _      -> "Time bounds are not a subset"
    ExpiresAfterNotBefore _ _      -> "Time bounds are not a subset"
    DecreasingVersionInChain a b   -> "Lower-version UCAN (v" <> display b <> ") depends on higher-version UCAN (v" <> display a <> ")"
