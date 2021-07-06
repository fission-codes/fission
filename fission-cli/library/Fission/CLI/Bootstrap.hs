module Fission.CLI.Bootstrap (module Fission.CLI.Bootstrap.Types) where

import qualified RIO.File                    as File
import           RIO.FilePath                ((</>))

import qualified Codec.Archive.Tar           as Tar
import qualified Codec.Compression.GZip      as GZip

import qualified Network.HTTP.Client         as HTTP

import qualified Turtle.Prelude              as Turtle

import           Fission.Prelude

import           Fission.CLI.Bootstrap.Types



downloadIPFS = do
  manager <- asks httpManager
  liftIO (HTTP.httpLbs (fromString "google.com") manager) >>= \case -- FIXME
    Left err ->
      undefined -- FIXME

    Right tarGz -> do
      File.writeBinaryFileDurableAtomic "/tmpDump" $ GZip.decompress tarGz
      liftIO $ Tar.extract "/tmpDump" "tmpDump" -- FIXME
      Turtle.mv ("/tmpDump" </> "go-ipfs" </> "ipfs") finalPath -- FIXME
