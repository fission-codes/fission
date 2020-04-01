-- |

module Fission.Web.Auth.JWT.Header.Types
  ( Header    (..)

  -- * Reexports

  , module Fission.Web.Auth.JWT.Header.Algorithm.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Auth.JWT.Header.Algorithm.Types
import           Fission.Web.Auth.JWT.Header.Cty.Types
import           Fission.Web.Auth.JWT.Header.Typ.Types

data Header = Header
  { typ :: !Typ
  , alg :: !Algorithm
  , cty :: !(Maybe Cty)
  } deriving (Show, Eq)

instance ToJSON Header where
  toJSON Header {..} = object
    [ "typ" .= typ
    , "alg" .= alg
    , "cty" .= cty
    ]

instance FromJSON Header where
  parseJSON = withObject "JWT.Header" \obj -> do
    typ <- obj .: "typ"
    alg <- obj .: "alg"
    cty <- obj .: "cty"
    return Header {..}
