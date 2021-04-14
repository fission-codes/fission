module Fission.Web.API.Auth.UCAN.Verify.Types (Verify) where

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

import           Fission.Web.API.Prelude

type Verify
  =  Summary "Verify a UCAN"
  :> Description "Check if a UCAN is validly formatted"
  --
  :> ReqBody '[PlainText] Bearer.BareToken
  :> QueryFlag "ignore_time"
  --
  :> PostNoContent
