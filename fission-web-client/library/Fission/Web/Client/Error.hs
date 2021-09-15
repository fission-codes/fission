module Fission.Web.Client.Error
  ( retryOnStatus
  , checkStatus
  ) where

import           Network.HTTP.Types.Status
import           Servant.Client

import           Fission.Prelude           hiding (fromMaybe)

import           Fission.Error
import           Fission.Web.Client

retryOnStatus ::
  ( MonadLogger    m
  , MonadWebClient m
  )
  => [Status]
  -> Natural
  -> m (ClientM a)
  -> m (Either ClientError a)
retryOnStatus retryOn times mkReq =
  retryOnErr (checkStatus retryOn) times do
    req <- mkReq
    sendRequest req

checkStatus :: MonadLogger m => [Status] -> Either ClientError a -> m Bool
checkStatus retryOn = \case
  Right _ ->
    return True

  Left (FailureResponse _req res) ->
    let
      code = responseStatusCode res
    in
      if elem code retryOn
        then do
          logWarn $ "Got a " <> textShow code <> "; retrying..."
          return False

        else
          return True

  Left _ ->
    return True
