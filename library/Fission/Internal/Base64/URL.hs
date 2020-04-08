-- | Helpers for working with Base64URL-encoded strings

module Fission.Internal.Base64.URL (decode, encode) where

import qualified RIO.Text.Partial as Text.Partial
 
import           Fission.Prelude hiding (encode, decode)

-- | Go from Base64URL to Base64
decode :: Text -> Text
decode = Text.Partial.replace "-" "+" . Text.Partial.replace "_" "/"

-- | Go from Base64 to Base64URL
encode :: Text -> Text
encode = Text.Partial.replace "+" "-" . Text.Partial.replace "/" "_"
