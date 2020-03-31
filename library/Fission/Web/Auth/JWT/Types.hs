module Fission.Web.Auth.JWT.Types
  ( Token  (..)

  -- * reexports
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where


import qualified Crypto.PubKey.Ed25519 as Ed

import           Fission.Prelude

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types

data Token = Token
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Ed.Signature -- FIXME: Ed25519 *OR* RSA
  } deriving (Show, Eq)
