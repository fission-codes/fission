module Fission.Web.Server.Link (mkLink) where

import           Servant               hiding (route)

import qualified Fission.Web.API.Types as Fission

mkLink :: (IsElem route Fission.API, HasLink route) => Proxy route -> MkLink route Link
mkLink pxy = safeLink (Proxy @Fission.API) pxy
