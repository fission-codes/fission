module Fission.Web.API.Auth.UCAN.Types (UCAN) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.Auth.UCAN.Verify.Types

type UCAN = "ucan" :> Verify
