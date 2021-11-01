module Network.IPFS.Pin
  ( add
  , rm
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Remote.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import qualified Network.IPFS.Client.Pin     as Pin
import           Network.IPFS.Add.Error      as IPFS.Add
import           Network.IPFS.Types          as IPFS
import           Servant.Client

-- | Pin a CID
add :: (MonadRemoteIPFS m, MonadLogger m) => IPFS.CID -> m (Either IPFS.Add.Error CID)
add cid = ipfsPin cid >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid'] -> do
        logDebug <| "Pinned CID " <> display cid'
        return <| Right cid'

      _ -> do
        formattedErr <- parseUnexpectedOutput <| UTF8.textShow cids
        return <| Left formattedErr

  Left err -> do
    formattedError <- parseClientError err
    return <| Left formattedError

-- | Unpin a CID
rm :: (MonadRemoteIPFS m, MonadLogger m) => IPFS.CID -> m (Either IPFS.Add.Error CID)
rm cid = ipfsUnpin cid False >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid'] -> do
        logDebug <| "Unpinned CID " <> display cid'
        return <| Right cid'

      _ -> do
        formattedErr <- parseUnexpectedOutput <| UTF8.textShow cids
        return <| Left formattedErr

  Left _ -> do
    logDebug <| "Cannot unpin CID " <> display cid <> " because it was not pinned"
    return <| Right cid

-- | Parse and Log the Servant Client Error returned from the IPFS Daemon
parseClientError :: MonadLogger m => ClientError -> m Error
parseClientError err = do
  logError <| displayShow err
  return <| case err of
    FailureResponse _ response ->
      response
        |> responseBody
        |> decode
        |> \case
          Just IPFS.ErrorBody {message} ->
            IPFSDaemonErr <| UTF8.textShow message

          _ ->
            UnexpectedOutput <| UTF8.textShow err

    unknownClientError ->
      UnknownAddErr <| UTF8.textShow unknownClientError

-- | Parse and Log unexpected output when attempting to pin
parseUnexpectedOutput :: MonadLogger m => Text -> m IPFS.Add.Error
parseUnexpectedOutput errStr = do
  let
    baseError = UnexpectedOutput errStr
    err = UnknownAddErr <| UTF8.textShow baseError

  logError <| display baseError
  return err
