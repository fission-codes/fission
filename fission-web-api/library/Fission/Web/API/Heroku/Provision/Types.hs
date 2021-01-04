module Fission.Web.API.Heroku.Provision.Types (Provision) where

import qualified Fission.Platform.Heroku.Provision.Request.Types as Heroku
import qualified Fission.Platform.Heroku.Provision.Types         as Heroku

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types                      as Auth
import qualified Fission.Web.API.Heroku.MIME.VendorJSONv3.Types  as Heroku

type Provision
  =  Auth.HerokuAddOnAPI
  --
  :> Summary "Provision"
  :> Description "Provision a new Heroku add-on (for the Heroku partner service only)"
  --
  :> ReqBody '[JSON]                Heroku.Request
  :> Post    '[Heroku.VendorJSONv3] Heroku.Provision
