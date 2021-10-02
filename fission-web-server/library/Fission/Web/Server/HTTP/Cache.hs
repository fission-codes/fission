module Fission.Web.Server.HTTP.Cache
  ( purgeMany
  , module Fission.Web.Server.HTTP.Cache.Class
  , module Fission.Web.Server.HTTP.Cache.Error
  ) where

import           RIO.NonEmpty

import           Fission.Prelude

import           Fission.URL
import           Fission.Web.Server.HTTP.Cache.Class
import           Fission.Web.Server.HTTP.Cache.Error

purgeMany :: MonadHTTPCache m => [URL] -> m (Either BatchErrors ())
purgeMany urls =
  foldM mAcc (Right ()) urls
  where
    mAcc acc url =
      purgeURL url >>= \case
        Right () ->
          return acc

        Left err ->
          let
            newErrs =
              case acc of
                Right _                     -> err :| []
                Left BatchErrors { errors } -> err `cons` errors

          in
            return $ Left BatchErrors {errors = newErrs}
