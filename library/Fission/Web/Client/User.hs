module Fission.Web.Client.User
  ( register
  , Verify
  , verify
  , updatePublicKey
  , whoami
  ) where

import           Servant hiding (addHeader)
-- import           Servant.Client

-- import           Fission.Prelude
 
-- import qualified Fission.Key as Key

-- import qualified Fission.User.Registration.Types as User
-- import qualified Fission.User.Username.Types     as User

-- import           Fission.Web.Client
import qualified Fission.Web.User   as User
import           Fission.Web.Routes (UserPrefix)

register :: Proxy (UserPrefix :> User.RegisterRoute)
register = Proxy

-- register :: User.Registration -> ClientM NoContent
-- register = registerClient $ Proxy @(UserPrefix :> User.RegisterRoute)

type Verify = UserPrefix :> User.VerifyRoute

verify :: Proxy Verify
verify = Proxy

-- verify :: ClientM Bool
-- verify = sigClient' $ Proxy @(UserPrefix :> User.VerifyRoute)

whoami :: Proxy (UserPrefix :> User.WhoAmIRoute)
whoami = Proxy

-- whoami :: ClientM User.Username
-- whoami = sigClient' $ Proxy @(UserPrefix :> User.WhoAmIRoute)

updatePublicKey :: Proxy (UserPrefix :> User.UpdatePublicKeyRoute)
updatePublicKey = Proxy

-- updatePublicKey :: BasicAuthData -> (Key.Public, Key.Algorithm) -> ClientM NoContent
-- updatePublicKey = basicClient $ Proxy @(UserPrefix :> User.UpdatePublicKeyRoute)
