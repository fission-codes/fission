module Fission.Web.Auth.Token.JWT.RawContent
  ( contentOf
  , module Fission.Web.Auth.Token.JWT.RawContent.Types
  ) where

import qualified RIO.Text                                    as Text

import           Fission.Prelude
import           Fission.Web.Auth.Token.JWT.RawContent.Types

-- | Smart constructor for 'RawContent'
contentOf :: Text -> RawContent
contentOf = RawContent . Text.dropEnd 1 . Text.dropWhileEnd (not . (== '.'))
