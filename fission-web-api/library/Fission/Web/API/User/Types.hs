module Fission.Web.API.User.Types (RoutesV_ (..), RoutesV2 (..)) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Relay.Types               as Relay
import qualified Fission.Web.API.User.Create.Types         as Create
import qualified Fission.Web.API.User.DID.Types            as DID
import qualified Fission.Web.API.User.DataRoot.Types       as DataRoot
import qualified Fission.Web.API.User.Email.Types          as Email
import qualified Fission.Web.API.User.ExchangeKey.Types    as ExchangeKeys
import qualified Fission.Web.API.User.Password.Reset.Types as Password
import           Fission.Web.API.User.Verify.Types
import qualified Fission.Web.API.User.WhoAmI.Types         as WhoAmI

data RoutesV2 mode = RoutesV2
  { dataRoot     :: mode :- "data"   :> ToServantApi DataRoot.RoutesV2
  , email        :: mode :- "email"  :> ToServantApi Email.Routes
  , did          :: mode :- "did"    :> ToServantApi DID.Routes
  , whoAmI       :: mode :- "whoami" :> ToServantApi WhoAmI.Routes
  , linkingRelay :: mode :- "link"   :> ToServantApi Relay.Routes
  , create       :: mode :- Create.WithDID
  }
  deriving Generic

data RoutesV_ mode = RoutesV_
  { whoAmI        :: mode :- "whoami"             :> ToServantApi WhoAmI.Routes
  , email         :: mode :- "email"              :> ToServantApi Email.Routes
  , did           :: mode :- "did"                :> ToServantApi DID.Routes
  , exchangeKeys  :: mode :- "exchange" :> "keys" :> ToServantApi ExchangeKeys.Routes
  , linkingRelay  :: mode :- "link"               :> ToServantApi Relay.Routes
  , dataRoot      :: mode :- "data"               :> ToServantApi DataRoot.RoutesV_
  , passwordReset :: mode :- Password.Reset
  , verify        :: mode :- Verify
  , create        :: mode :- ToServantApi Create.RoutesV_
  }
  deriving Generic
