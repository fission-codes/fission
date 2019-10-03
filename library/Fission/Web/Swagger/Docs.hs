module Fission.Web.Swagger.Docs
  ( app
  , auth
  , ipfs
  , heroku
  , ping
  , register
  ) where

import RIO

import Control.Lens
import Data.Swagger
import Servant.Swagger

import           Fission.Internal.Orphanage.BasicAuth ()
import           Fission.Internal.Orphanage.MultipartForm ()
import qualified Fission.Web.Routes                       as Web

app :: HasSwagger api => Proxy api -> Host -> Swagger
app proxy appHost = toSwagger proxy
                  & host               ?~ appHost
                  & schemes            ?~ [Https, Http]
                  & info . title       .~ "FISSION's IPFS API"
                  & info . version     .~ "1.0.0"
                  & info . description ?~ "Easily use IPFS from Web 2.0 applications"
                  & info . contact     ?~ fissionContact
                  & info . license     ?~ projectLicense
  where
    fissionContact = mempty
                   & name  ?~"FISSION Team"
                   & url   ?~ URL "https://fission.codes"
                   & email ?~ "support@fission.codes"

    projectLicense = "Apache 2.0"
                   & url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0"

auth :: Swagger -> Swagger
auth = applyTagsFor ops  ["Authentication" & description ?~ "Auth actions & verification"]
  where
    ops = subOperations (Proxy :: Proxy Web.AuthRoute) (Proxy :: Proxy Web.API)

heroku :: Swagger -> Swagger
heroku = applyTagsFor ops  ["Heroku" & description ?~ "Interaction with the Heroku add-on API"]
  where
    ops = subOperations (Proxy :: Proxy Web.HerokuRoute) (Proxy :: Proxy Web.API)

ipfs :: Swagger -> Swagger
ipfs = applyTagsFor ops ["IPFS" & description ?~ "The primary IPFS API"]
  where
    ops = subOperations (Proxy :: Proxy Web.IPFSRoute) (Proxy :: Proxy Web.API)

ping :: Swagger -> Swagger
ping = applyTagsFor ops  ["Ping" & description ?~ "Check for liveness"]
  where
    ops = subOperations (Proxy :: Proxy Web.PingRoute) (Proxy :: Proxy Web.API)

register :: Swagger -> Swagger
register = applyTagsFor ops  ["Register" & description ?~ "Register and provision a user"]
  where
    ops = subOperations (Proxy :: Proxy Web.RegisterRoute) (Proxy :: Proxy Web.API)
