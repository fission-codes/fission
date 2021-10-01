module Fission.Web.Server.Internal.Varnish.Purge
  ( purge
  , purgeMany
  , module Fission.Web.Server.Internal.Varnish.Purge.Error
  , module Fission.Web.Server.Internal.Varnish.Purge.Types
  ) where

import           Network.HTTP.Req
import           RIO.NonEmpty

import           Fission.Prelude

import           Fission.URL.Types
import           Fission.Web.Server.Error.Class

import qualified Fission.Web.Server.Internal.Varnish.Purge.Error as Varnish

-- Reexports

import           Fission.Web.Server.Internal.Varnish.Purge.Error
import           Fission.Web.Server.Internal.Varnish.Purge.Types

-- | Purge a URL from the Varnish cache
-- Varnish docs: https://docs.nginx.com/nginx/admin-guide/content-cache/content-caching/#purge_request
purge :: (MonadLogger m, MonadHttp m) => URL -> m (Either Varnish.Error ())
purge url = do
  let url' = https (textDisplay url) /: "*"
  resp <- req PURGE url' NoReqBody ignoreResponse mempty
  logInfo $ "🔥 Purging cache for " <> displayShow url'
  let status = responseStatusCode resp
  if status >= 400
    then do
      logError @Text "FAILED PURGE!"
      return . Left . Varnish.Error $ toServerError status

    else do
      logInfo @Text "SUCCESS!!"
      return $ Right ()

purgeMany :: forall m . (MonadLogger m, MonadHttp m) => [URL] -> m (Either Varnish.BatchErrors ())
purgeMany urls =
  foldM mAcc (Right ()) urls
  where
    mAcc :: Either Varnish.BatchErrors () -> URL -> m (Either Varnish.BatchErrors ())
    mAcc acc url =
      purge url >>= \case
        Right () ->
          return acc

        Left (Varnish.Error err) ->
          return . Left $ BatchErrors case acc of
            Right ()                      -> [err]
            Left (Varnish.BatchErrors errs) -> (err `cons` errs)
