module Fission.Error.ParseError.Types (ParseError (..)) where

import           Fission.Prelude

import           Web.DID.Types

data ParseError entity
  = ParseError
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ParseError DID) where
  display _ = "Unable to parse DID"