module Fission.Web.API
  ( docs
  , v2
  , v_
  , module Fission.Web.API.Types
  ) where

import qualified Fission.Web.API.App.Types    as App
import qualified Fission.Web.API.Auth.Types   as Auth
import qualified Fission.Web.API.DNS.Types    as DNS
import qualified Fission.Web.API.Heroku.Types as Heroku
import qualified Fission.Web.API.IPFS.Types   as IPFS
import qualified Fission.Web.API.Ping.Types   as Ping
import qualified Fission.Web.API.User.Types   as User
import           Fission.Web.API.Types

import           Fission.Web.API.Prelude

docs :: HasSwagger api => Proxy api -> Web.Host -> Swagger
docs proxy appHost verson =
  proxy
    |> toSwagger
    |> host               ?~ Host (Web.getRawHost appHost) Nothing
    |> schemes            ?~ [Https]
    |> info . title       .~ "The Fission API"
    |> info . version     .~ version'
    |> info . description ?~ blurb
    |> info . contact     ?~ fissionContact
    |> info . license     ?~ projectLicense
  where
    version' =
      Text.pack $ showVersion Fission.version

    fissionContact =
      mempty
        |> name  ?~ "Team Fission"
        |> url   ?~ URL "https://fission.codes"
        |> email ?~ "support@fission.codes"

    projectLicense =
      "AGPL 3.0" |> url ?~ URL "https://www.gnu.org/licenses/agpl-3.0.en.html"

    blurb =
      "Bootstrapped & distributed backend-as-a-service with user-controlled data"

v2 :: Swagger -> Swagger
v2 = App.docs . Auth.docs . DNS.docs . Heroku.docs . IPFS.docs . Ping.docs . User.docs

v_ :: Swagger -> Swagger
v_ = App.docs . Auth.docs . DNS.docs . Heroku.docs . IPFS.docs . Ping.docs . User.docs
