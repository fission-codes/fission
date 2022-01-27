module Fission.Web.Auth.Token.Bearer
  ( fromJWT
  , toProof
  -- * Reexports
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import qualified RIO.ByteString.Lazy                 as Lazy

import qualified Web.UCAN.Types                      as UCAN

import           Fission.Prelude

import           Fission.Web.Auth.Token.UCAN         as UCAN
import           Fission.Web.Auth.Token.UCAN.Types   as Fission

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

fromJWT :: Fission.UCAN -> Token
fromJWT jwt =
  Token { jwt
        , rawContent = contentOf . decodeUtf8Lenient . Lazy.toStrict $ encode jwt
        }

toProof :: Maybe Token -> Proof
toProof Nothing           = UCAN.RootCredential
toProof (Just Token {..}) = UCAN.Nested rawContent jwt
