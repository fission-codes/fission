module Fission.Web.API.Docs
  ( meta
  , module Fission.Web.API.Docs.Types
  ) where

import           Data.Version               (Version, showVersion)
import qualified RIO.Text                   as Text

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.Prelude    hiding (Https)

import           Fission.Web.API.Docs.Types
import qualified Fission.Web.API.Host.Types as Web

meta :: HasSwagger api => Proxy api -> Version -> Web.Host -> Swagger
meta proxy v appHost =
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
      Text.pack $ showVersion v

    fissionContact =
      mempty
        |> name  ?~ "Team Fission"
        |> url   ?~ URL "https://fission.codes"
        |> email ?~ "support@fission.codes"

    projectLicense =
      "AGPL 3.0" |> url ?~ URL "https://www.gnu.org/licenses/agpl-3.0.en.html"

    blurb =
      "Bootstrapped & distributed backend-as-a-service with user-controlled data"
