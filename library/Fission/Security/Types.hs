-- | Application security
module Fission.Security.Types
  ( Secret (..)
  , SecretDigest
  ) where

import RIO

import Control.Lens
import Data.Aeson
import Data.Swagger

-- | A text digest
type SecretDigest = Text

-- | An application secret
newtype Secret = Secret { unSecret :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  ( FromJSON
                    , ToJSON
                    )

instance ToSchema Secret where
  declareNamedSchema _ =
     return $ NamedSchema (Just "Secret") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ "U)mRvIvI6$L_MkYpme!lfzMte_92M5G912-NUfRmfxhRKx$Rr6aLUxqdqW"
            & description ?~ "User secret (used for authentication)"
