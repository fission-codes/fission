module Fission.Web.API.User.Types (User) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.User.Create.Types
import           Fission.Web.API.User.DID.Types
import           Fission.Web.API.User.DataRoot.Types
import           Fission.Web.API.User.Email.Types
import           Fission.Web.API.User.ExchangeKey.Types
import qualified Fission.Web.API.User.Password.Reset.Types as Password
import           Fission.Web.API.User.Verify.Types
import           Fission.Web.API.User.WhoAmI.Types

type User =  "user" :> API

type API
  =   Create
 :<|> WhoAmI
 :<|> Verify
 :<|> Email
 :<|> DID
 :<|> ExchangeKeys
 :<|> DataRoot
 :<|> Password.Reset
