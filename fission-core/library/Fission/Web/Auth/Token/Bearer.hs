module Fission.Web.Auth.Token.Bearer
  ( fromJWT
  , toProof
  -- * Reexports
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import qualified RIO.ByteString.Lazy                 as Lazy

import qualified Web.JWT.Types                       as JWT

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

fromJWT :: FissionJWT -> Token
fromJWT jwt =
  Token { jwt
        , rawContent = contentOf . decodeUtf8Lenient . Lazy.toStrict $ encode jwt
        }

toProof :: Maybe Token -> Proof
toProof Nothing           = JWT.RootCredential
toProof (Just Token {..}) = JWT.Nested rawContent jwt
