module Fission.Error.ParseError.Types (ParseError (..)) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.UCAN.Types

import           Fission.Web.Auth.Token.UCAN.Potency.Types

import           Web.DID.Types

data ParseError entity
  = ParseError
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ParseError DID) where
  display _ = "Unable to parse DID"

instance Display (ParseError UCAN) where
  display _ = "Unable to parse UCAN"

instance Display (ParseError Potency) where
  display _ = "Unable to parse Potency"