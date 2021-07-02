module Fission.Web.Server.Internal.NGINX.Purge
  ( purge
  , purgeMany
  , module Fission.Web.Server.Internal.NGINX.Purge.Error
  , module Fission.Web.Server.Internal.NGINX.Purge.Types
  ) where

import           Network.HTTP.Req
import           RIO.NonEmpty

import           Fission.Prelude

import           Fission.URL.Types
import           Fission.Web.Server.Error.Class

import qualified Fission.Web.Server.Internal.NGINX.Purge.Error as NGINX

-- Reexports

import           Fission.Web.Server.Internal.NGINX.Purge.Error
import           Fission.Web.Server.Internal.NGINX.Purge.Types

-- | Purge a URL from the NGINX cache
-- NGINX docs: https://docs.nginx.com/nginx/admin-guide/content-cache/content-caching/#purge_request
purge :: MonadHttp m => URL -> m (Either NGINX.Error ())
purge url = do
  resp <- req PURGE (https $ textDisplay url) NoReqBody ignoreResponse mempty
  let status = responseStatusCode resp
  if status >= 400
    then return . Left . NGINX.Error $ toServerError status
    else return $ Right ()

purgeMany :: forall m . MonadHttp m => [URL] -> m (Either NGINX.BatchErrors ())
purgeMany urls =
  foldM mAcc (Right ()) urls
  where
    mAcc :: Either NGINX.BatchErrors () -> URL -> m (Either NGINX.BatchErrors ())
    mAcc acc url =
      purge url >>= \case
        Right () ->
          return acc

        Left (NGINX.Error err) ->
          return . Left $ BatchErrors case acc of
            Right ()                      -> [err]
            Left (NGINX.BatchErrors errs) -> (err `cons` errs)
