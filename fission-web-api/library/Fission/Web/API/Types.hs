-- | Top-level router type for the application
module Fission.Web.API.Types
  ( Routes   (..)
  , RoutesV2 (..)
  , RoutesV_ (..)
  , V2       (..)
  , Root
  ) where

import           Servant.API.Generic

import qualified Fission.Pong.Types              as Ping

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.App.Types       as App
import qualified Fission.Web.API.Auth.UCAN.Types as UCAN
import qualified Fission.Web.API.DNS.Types       as DNS
import           Fission.Web.API.Docs
import qualified Fission.Web.API.Heroku.Types    as Heroku
import qualified Fission.Web.API.IPFS.Types      as IPFS
import qualified Fission.Web.API.User.Types      as User

data Routes mode = Routes
  { v2          :: mode :- "v2" :> ToServantApi RoutesV2
  , latestDocs  :: mode :- Docs
  , unversioned :: mode :- ToServantApi RoutesV_
  , root        :: mode :- Root
  }
  deriving Generic

type Root = Get '[JSON, OctetStream, PlainText] NoContent

data RoutesV2 mode = RoutesV2
  { api  :: mode :- "api" :> ToServantApi V2
  , docs :: mode :- Docs
  }
  deriving Generic

data V2 mode = V2
  { ipfs   :: mode :- "ipfs"                  :> ToServantApi IPFS.RoutesV2
  , app    :: mode :- "app"                   :> ToServantApi App.Routes
  , heroku :: mode :- "heroku" :> "resources" :> ToServantApi Heroku.Routes
  , user   :: mode :- "user"                  :> ToServantApi User.RoutesV2
  , auth   :: mode :- "auth"   :> "ucan"      :> ToServantApi UCAN.Routes
  }
  deriving Generic

-- DEPRECATED version
data RoutesV_ mode = RoutesV_
  { ipfs   :: mode :- "ipfs"                  :> ToServantApi IPFS.RoutesV_
  , app    :: mode :- "app"                   :> ToServantApi App.Routes
  , heroku :: mode :- "heroku" :> "resources" :> ToServantApi Heroku.Routes
  , user   :: mode :- "user"                  :> ToServantApi User.RoutesV_
  , dns    :: mode :- "dns"                   :> ToServantApi DNS.Routes
  , auth   :: mode :- "auth"   :> "ucan"      :> ToServantApi UCAN.Routes

  , ping ::
      mode
      :- Summary "Simple Ping"
      :> Description "DEPRECATED ⛔ A quick way to check for liveness"
      --
      :> "ping"
      :> Get '[JSON, PlainText] Ping.Pong
  }
  deriving Generic
