module Fission.Internal.API (mkLink) where

import Fission.Prelude 
import Servant         hiding (route)

import qualified Fission.Web.Routes as Web 

mkLink :: (IsElem route Web.API, HasLink route) => Proxy route -> MkLink route Link
mkLink pxy = safeLink (Proxy @Web.API) pxy
