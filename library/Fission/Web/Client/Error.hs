module Fission.Web.Client.Error (retryOnErr) where 

import Fission.Prelude hiding (fromMaybe)

import Fission.Web.Client
import Servant.Client
import Network.HTTP.Types.Status

retryOnErr ::
  ( MonadWebClient m
  , MonadLogger m
  )
  => [Status]
  -> Integer
  -> ClientM a
  -> m (Either ClientError a)
retryOnErr retryOn times req =
  sendRequest req >>= \case
    Right val ->
      return $ Right val

    Left err@(FailureResponse _req res) -> do
      let code = responseStatusCode res

      if (elem (responseStatusCode res) retryOn) && times > 0
        then do
          logWarn $ "Got a " <> textShow code <> "; retrying..."
          retryOnErr retryOn (times - 1) req
        
        else
          return $ Left err

    Left err ->
      return $ Left err


