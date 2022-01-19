module Fission.Web.Auth.Token.Bearer
  ( fromJWT
  , toProof
  -- * Reexports
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import qualified RIO.ByteString.Lazy                 as Lazy

import qualified Web.Ucan.Types                      as Ucan

import           Fission.Prelude

import           Fission.Web.Auth.Token.Ucan         as Ucan
import           Fission.Web.Auth.Token.Ucan.Types   as Fission

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

fromJWT :: Fission.Ucan -> Token
fromJWT jwt =
  Token { jwt
        , rawContent = contentOf . decodeUtf8Lenient . Lazy.toStrict $ encode jwt
        }

toProof :: Maybe Token -> Proof
toProof Nothing           = Ucan.RootCredential
toProof (Just Token {..}) = Ucan.Nested rawContent jwt
