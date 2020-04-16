-- | JOSE @"cty"@ (Content Type) Header Parameter

module Fission.Web.Auth.Token.JWT.Header.Cty.Types (Cty (..)) where

import Fission.Prelude

{- |

RFC 7519
https://tools.ietf.org/html/rfc7519

5.2.  "cty" (Content Type) Header Parameter

   The "cty" (content type) Header Parameter defined by [JWS] and [JWE]
   is used by this specification to convey structural information about
   the JWT.

   In the normal case in which nested signing or encryption operations
   are not employed, the use of this Header Parameter is NOT
   RECOMMENDED.  In the case that nested signing or encryption is
   employed, this Header Parameter MUST be present; in this case, the
   value MUST be "JWT", to indicate that a Nested JWT is carried in this
   JWT.  While media type names are not case sensitive, it is
   RECOMMENDED that "JWT" always be spelled using uppercase characters
   for compatibility with legacy implementations.  See Appendix A.2 for
   an example of a Nested JWT.

-}
data Cty
  = JWT
  deriving (Eq, Show, Read)

instance Arbitrary Cty where
  arbitrary = return JWT

instance ToJSON Cty where
  toJSON JWT = String "JWT"

instance FromJSON Cty where
  parseJSON = withText "JWT.Header.Cty" \case
    "JWT" -> return JWT
    "jwt" -> return JWT
    other -> fail $ show other <> "is not a valid JWT 'cty' value for Fission"
