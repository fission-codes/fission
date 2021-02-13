module Fission.Web.Auth.Token.Bearer
  ( toProof
  -- * Reexports
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT          (Proof (..))

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

toProof :: Maybe Token -> Proof
toProof Nothing           = RootCredential
toProof (Just Token {..}) = Nested rawContent jwt
