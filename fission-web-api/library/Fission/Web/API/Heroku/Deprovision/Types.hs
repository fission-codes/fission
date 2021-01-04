module Fission.Web.API.Heroku.Deprovision.Types (Deprovision) where

import           Data.UUID

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types                     as Auth
import qualified Fission.Web.API.Heroku.MIME.VendorJSONv3.Types as Heroku

type Deprovision
  = Auth.HerokuAddOnAPI
  --
  :> Summary "Deprovision"
  :> Description "Deprovision a Heroku add-on (for the Heroku partner service only)"
  --
  :> Capture "addon_id" UUID
  :> Delete '[Heroku.VendorJSONv3] NoContent
