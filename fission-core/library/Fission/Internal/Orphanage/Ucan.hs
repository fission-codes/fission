{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ucan () where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Error.NotFound.Types


instance Display (NotFound FissionJWT) where
  display _ = "Unable to find UCAN"

instance Display (NotFound Resource) where
  display _ = "No UCAN resource provided (closed UCAN)"
