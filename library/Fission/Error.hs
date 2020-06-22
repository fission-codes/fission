module Fission.Error
  ( openLeft
  , relaxedLeft
  , fromMaybe
  , fromMaybe'
  , retryOnErr
  , module Fission.Error.Types
  ) where

import Fission.Prelude hiding (fromMaybe)
import Fission.Error.Types

import Fission.Web.Client
import Servant.Client
import Network.HTTP.Types.Status


openLeft :: IsMember err errs => err -> Either (OpenUnion errs) a
openLeft err = Left (openUnionLift err)

relaxedLeft :: Contains errsA errsB => OpenUnion errsA -> Either (OpenUnion errsB) a
relaxedLeft = Left . relaxOpenUnion

fromMaybe ::
  IsMember err errs
  => err
  -> (a -> b)
  -> Maybe a
  -> Either (OpenUnion errs) b
fromMaybe err okHandler = maybe (openLeft err) (Right . okHandler)

fromMaybe' :: IsMember err errs => err -> Maybe a -> Either (OpenUnion errs) a
fromMaybe' err = fromMaybe err identity

retryOnErr ::
  ( MonadWebClient m
  , MonadLogger m
  )
  => [Status]
  -> Integer
  -> m (ClientM a)
  -> m (Either ClientError a)
retryOnErr retryOn times req =
  sendRequestM req >>= \case
    Right val ->
      return $ Right val

    Left err@(FailureResponse _req res) -> do
      let code = responseStatusCode res
 
      if elem (responseStatusCode res) retryOn
        then do
          logWarn $ "Got a " <> textShow code <> "; retrying..."
          retryOnErr retryOn (times - 1) req

        else
          return $ Left err

    Left err ->
      return $ Left err


