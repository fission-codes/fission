module Fission.Web.Server.Link (mkLink) where

import           Servant               hiding (route)
import           Servant.API.Generic

import qualified Fission.Web.API.Types as Fission

type BasicAPI = ToServantApi Fission.Routes

mkLink :: (IsElem route BasicAPI, HasLink route) => Proxy route -> MkLink route Link
mkLink pxy = safeLink (Proxy @BasicAPI) pxy
