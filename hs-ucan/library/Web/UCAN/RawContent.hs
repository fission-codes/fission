module Web.UCAN.RawContent
  ( contentOf
  , module Web.UCAN.RawContent.Types
  ) where

import           RIO
import qualified RIO.Text                  as Text

import           Web.UCAN.RawContent.Types

-- | Smart constructor for 'RawContent'
contentOf :: Text -> RawContent
contentOf = RawContent . stripOptionalPrefix "\"" . Text.dropEnd 1 . Text.dropWhileEnd (not . (== '.'))
  where
    stripOptionalPrefix pfx txt = maybe txt id $ Text.stripPrefix pfx txt
