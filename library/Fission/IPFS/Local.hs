module Fission.IPFS.Local (toBinPath) where

import qualified RIO.ByteString.Lazy        as Lazy
import qualified RIO.Text                   as Text

import qualified Network.IPFS.BinPath.Types as IPFS
import qualified RIO.Process                as Process

import           Fission.Prelude

import qualified Fission.Internal.UTF8      as UTF8

toBinPath ::
  ( HasProcessContext cfg
  , HasLogFunc        cfg
  , MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  )
  => Maybe IPFS.BinPath
  -> m IPFS.BinPath
toBinPath (Just binPath) =
  pure binPath

toBinPath Nothing = do
  (exitCode, systemIPFS, errMsg) <- Process.proc "which" ["ipfs"] Process.readProcess

  case exitCode of
    ExitSuccess ->
      return . IPFS.BinPath . Text.unpack . decodeUtf8Lenient . Lazy.toStrict $ UTF8.stripNewline systemIPFS

    ExitFailure _ -> do
      logDebug errMsg
      logDebug @Text "No IPFS binary on the $PATH; falling back to default."
      return $ IPFS.BinPath "/usr/local/bin/ipfs"
