module Fission.AWS.CertManager.Error (Error (..)) where

import           Fission.Prelude
import           Servant.Server
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data Error = NoARN
  deriving ( Show
           , Exception
           , Eq
           )

instance Display Error where
  display NoARN = "No CertificateARN received from AWS"

instance ToServerError Error where
  toServerError NoARN =
    err500 { errBody = UTF8.showLazyBS <| textDisplay NoARN }
