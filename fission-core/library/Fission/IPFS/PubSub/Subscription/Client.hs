module Fission.IPFS.PubSub.Subscription.Client
  ( runMessageStream
  , messageStreamClient
  , module Fission.IPFS.PubSub.Subscription.Client.Types
  ) where

import           Control.Monad.Catch                            as Catch

import           Servant.Client.Streaming                       as Streaming
import           Servant.Types.SourceT

import           Fission.Prelude

import           Fission.IPFS.PubSub.Subscription.Client.Types
import           Fission.IPFS.PubSub.Subscription.Message.Types

runMessageStream ::
  forall m msg .
  ( MonadIO    m
  , MonadRaise m
  , m `Raises` ClientError
  , m `Raises` SomeException
  , FromJSON msg
  )
  => ClientEnv
  -> TQueue (Either String (Message msg)) -- FIXME better error type
  -> m ()
runMessageStream env tq =
  liftIO go >>= \case
    Right ()         -> return ()
    Left (Left err)  -> raise err
    Left (Right err) -> raise err

  where
    go :: IO (Either (Either SomeException ClientError) ())
    go =
      Catch.catch (Right <$> streamClient) \err ->
        case Catch.fromException err of
          Nothing   -> return . Left $ Left err
          Just err' -> return . Left $ Right (err' :: ClientError)

    streamClient :: IO ()
    streamClient =
      Streaming.withClientM messageStreamClient env \case
        Left  err    -> throwM err
        Right stream -> foreach withErr withMsg stream

    withErr :: String -> IO ()
    withErr str = atomically $ writeTQueue tq (Left str)

    withMsg :: Message msg -> IO ()
    withMsg msg = atomically $ writeTQueue tq (Right msg)

messageStreamClient ::
  forall a m .
  ( FromJSON a
  , MonadIO m
  )
  => Streaming.ClientM (SourceT m (Message a))
messageStreamClient = Streaming.client (Proxy @(MessageStream m a))
