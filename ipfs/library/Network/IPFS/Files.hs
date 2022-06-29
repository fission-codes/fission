module Network.IPFS.Files (cp, rm, statCID, write) where

import           Data.ByteString.Lazy.Char8  as CL

import           Network.IPFS.Client.Files   as Files
import           Network.IPFS.File.Types
import           Network.IPFS.Files.Error    as IPFS.Files
import qualified Network.IPFS.Internal.UTF8  as UTF8
import           Network.IPFS.Remote.Class   as IPFS
import           Network.IPFS.Prelude        hiding (link)
import qualified Network.IPFS.Process.Error  as Process
import           Network.IPFS.CID.Types      as IPFS ( mkCID, CID(CID) )
import           Network.IPFS.Types          as IPFS

import qualified Network.IPFS.Client.Files.Statistics.Types as Files.Statistics
import qualified Network.IPFS.Client.Files.Write.Form.Types as Write

import qualified RIO.ByteString.Lazy         as Lazy
import qualified RIO.Text                    as Text

import           Servant.Client
import           Servant.Multipart           as Multipart
import qualified Servant.Multipart.Client    as Multipart.Client


{-| MFS Copying.

Can be used for both copying to and from MFS.
Will create parent directories.

-}
cp :: (MonadLogger m, MonadRemoteIPFS m)
  => Either IPFS.CID FilePath
  -> FilePath
  -> m (Either IPFS.Files.Error ())
cp cidOrFilePath destination =
  let
    from =
      case cidOrFilePath of
        Left (IPFS.CID hash) -> Text.unpack (Text.append "/ipfs/" hash)
        Right path           -> path

    args = MfsCopyArgs
        { from = Text.pack from
        , to = Text.pack destination
        , parents = True
        }
  in
  IPFS.mfsCopy args >>= \case
    Right () ->
      return $ Right ()

    Left err -> do
      formattedError <- parseClientError err
      return <| Left formattedError


{-| MFS Removing.

Remove something from MFS.

-}
rm :: (MonadLogger m, MonadRemoteIPFS m)
  => FilePath
  -> m (Either IPFS.Files.Error ())
rm path =
  let
    args = MfsRemoveArgs { path = Text.pack path, recursive = True, force = Nothing }
  in
  IPFS.mfsRemove args >>= \case
    Right () ->
      return $ Right ()

    Left err -> do
      formattedError <- parseClientError err
      return <| Left formattedError


{-| MFS CID for path.

Get the CID for a given path.

-}
statCID :: (MonadLogger m, MonadRemoteIPFS m)
  => FilePath
  -> m (Either IPFS.Files.Error IPFS.CID)
statCID path =
  let
    args = MfsStatArgs { path = Text.pack path }
  in
  IPFS.mfsStat args >>= \case
    Right (Files.Statistics.Response {cid}) ->
      return $ Right cid

    Left err -> do
      formattedError <- parseClientError err
      return <| Left formattedError


{-| MFS Writing.

Write to a file in MFS.

-}
write :: (MonadLogger m, MonadRemoteIPFS m)
  => FilePath
  -> Lazy.ByteString
  -> m (Either IPFS.Files.Error ())
write path raw =
  let
    args = MfsWriteArgs
        { path = Text.pack path
        , create = True
        , parents = True
        , truncate = True
        , rawLeaves = Just True
        , cidVersion = Just 1
        , hash = Nothing
        }
  in do
  boundary <- liftIO Multipart.Client.genBoundary
  let formData = Serialized raw

  IPFS.mfsWrite args (boundary, Write.Form formData) >>= \case
    Right () ->
      return $ Right ()

    Left err -> do
      formattedError <- parseClientError err
      return <| Left formattedError



-- 🛠


{-| Parse and Log the Servant Client Error returned from the IPFS Daemon
-}
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
      UnknownFilesErr <| UTF8.textShow unknownClientError