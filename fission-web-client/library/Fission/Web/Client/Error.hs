module Fission.Web.Client.Error
  ( retryOnStatus
  , checkStatus
  ) where

import           Fission.Error
import           Fission.Prelude           hiding (fromMaybe)

import           Fission.Web.Client
import           Network.HTTP.Types.Status
import           Servant.Client


retryOnStatus ::
  ( MonadWebClient m
  , MonadLogger m
  )
  => [Status]
  -> Natural
  -> ClientM a
  -> m (Either ClientError a)
retryOnStatus retryOn times req =
  retryOnErr (checkStatus retryOn) times (sendRequest req)

checkStatus ::
  MonadLogger m
  => [Status]
  -> Either ClientError a
  -> m Bool
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
