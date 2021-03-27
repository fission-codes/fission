module Fission.Web.Auth.Token.Bearer
  ( fromJWT
  , toProof
  -- * Reexports
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import qualified RIO.ByteString.Lazy                 as Lazy

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

fromJWT :: JWT -> Token
fromJWT jwt =
  Token { jwt
        , rawContent = contentOf . decodeUtf8Lenient . Lazy.toStrict $ encode jwt
        }

toProof :: Maybe Token -> Proof
toProof Nothing           = RootCredential
toProof (Just Token {..}) = Nested rawContent jwt
