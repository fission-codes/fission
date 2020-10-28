module Fission.Web.Auth.Token.JWT.RawContent.Class (ToRawContent (..)) where

import           Fission.Web.Auth.Token.JWT.RawContent.Types

-- | Create an unsigned base64 encoded token
class ToRawContent tokenable where
  toRawContent :: tokenable -> RawContent
