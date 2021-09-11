module Fission.Web.API.Auth.UCAN.Types (Routes (..)) where

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

import           Fission.Web.API.Prelude

newtype Routes mode = Routes
  { verify ::
      mode
       :- Summary "Verify a UCAN"
       :> Description "Check if a UCAN is validly formatted"
       --
       :> ReqBody '[PlainText] Bearer.BareToken
       :> QueryFlag "ignore_time"
       --
       :> PostNoContent
  }
  deriving Generic
