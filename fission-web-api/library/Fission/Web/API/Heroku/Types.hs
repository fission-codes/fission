module Fission.Web.API.Heroku.Types (Routes (..)) where

import           Data.UUID

import qualified Fission.Platform.Heroku.Provision.Request.Types as Heroku
import qualified Fission.Platform.Heroku.Provision.Types         as Heroku

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types                      as Auth
import qualified Fission.Web.API.Heroku.MIME.VendorJSONv3.Types  as Heroku

-- | Internal Heroku web API
data Routes mode = Routes
  { provision ::
      mode
      :- Summary "Provision"
      :> Description "Provision a new Heroku add-on (for the Heroku partner service only)"
      --
      :> ReqBody '[JSON] Heroku.Request
      --
      :> Auth.HerokuAddOnAPI
      :> Post '[Heroku.VendorJSONv3] Heroku.Provision

  , deprovision ::
      mode
      :- Summary "Deprovision"
      :> Description "Deprovision a Heroku add-on (for the Heroku partner service only)"
      --
      :> Capture "addon_id" UUID
      --
      :> Auth.HerokuAddOnAPI
      :> Delete '[Heroku.VendorJSONv3] NoContent
  }
  deriving Generic
