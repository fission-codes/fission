module Fission.Web.Auth.Token.JWT.RawContent.Class (ToRawContent (..)) where

import           Fission.Web.Auth.Token.JWT.RawContent.Types

-- | Create an unsigned base64 encoded token
class ToRawContent ucan where
  toRawToken :: ucan -> RawContent


        -- FIXME innerJWT@(UCAN {..}) <- arbitrary
        -- FIXME let rawContent = RawContent $ B64.URL.encodeJWT header claims
