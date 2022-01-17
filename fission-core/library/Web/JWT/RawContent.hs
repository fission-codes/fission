module Web.JWT.RawContent
  ( contentOf
  , module Web.JWT.RawContent.Types
  ) where

import qualified RIO.Text                 as Text

import           Fission.Prelude

import qualified Fission.Internal.UTF8    as UTF8

import           Web.JWT.RawContent.Types

-- | Smart constructor for 'RawContent'
contentOf :: Text -> RawContent
contentOf = RawContent . UTF8.stripOptionalPrefix "\"" . Text.dropEnd 1 . Text.dropWhileEnd (not . (== '.'))
