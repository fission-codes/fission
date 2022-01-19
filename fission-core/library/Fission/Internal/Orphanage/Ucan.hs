{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ucan () where

import           Fission.Prelude

import           Fission.Web.Auth.Token.Ucan.Resource.Types
import qualified Fission.Web.Auth.Token.Ucan.Types          as Fission

import           Fission.Error.NotFound.Types


instance Display (NotFound Fission.Ucan) where
  display _ = "Unable to find UCAN"

instance Display (NotFound Resource) where
  display _ = "No UCAN resource provided (closed UCAN)"
