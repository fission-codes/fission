module Fission.Web.API.Docs (makeDocs, subOps) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.Types

makeDocs
  :: ( Servant.API.IsSubAPI subRoute Fission.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> [Tag]
  -> Swagger
  -> Swagger
makeDocs routeProxy = applyTagsFor (subOps routeProxy)

subOps
  :: ( Applicative f
     , Servant.API.IsSubAPI subRoute Fission.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> (Operation -> f Operation)
  ->   Swagger
  -> f Swagger
subOps routeProxy = subOperations routeProxy $ Proxy @Fission.API
