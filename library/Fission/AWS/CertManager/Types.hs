module Fission.AWS.CertManager.Types (CertARN (..)) where

import Fission.Prelude

-- | Type safety wrapper for a CertificateARN
newtype CertARN = CertARN { getCertARN :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving newtype  ( IsString )
