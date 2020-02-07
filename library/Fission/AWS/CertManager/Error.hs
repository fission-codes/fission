module Fission.AWS.CertManager.Error (Error (..)) where

import           Fission.Prelude
import           Servant.Server
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data Error
  = NoARN
  | CertNotFound
  deriving ( Show
           , Exception
           , Eq
           )

instance Display Error where
  display = \case
    NoARN        -> "No CertificateARN received"
    CertNotFound -> "Could not find certificate"

instance ToServerError Error where
  toServerError = \case 
    NoARN        -> err500 { errBody = UTF8.showLazyBS <| textDisplay NoARN }
    CertNotFound -> err404 { errBody = UTF8.showLazyBS <| textDisplay CertNotFound }
