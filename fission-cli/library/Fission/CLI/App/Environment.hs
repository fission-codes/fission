module Fission.CLI.App.Environment
  ( create
  , read
  , readFrom
  , absPath
  , relPath
  , ignoreDefault
  , module Fission.CLI.App.Environment.Types
  ) where

import qualified Data.Yaml                         as YAML

import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                          as Text

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Fission.URL.Types

import           Fission.CLI.App.Environment.Types
import qualified Fission.CLI.YAML                  as YAML

create :: MonadIO m => URL -> FilePath -> m ()
create appURL buildDir = do
  path <- absPath
  YAML.writeFile path Env {ipfsIgnored = [], ..}

read ::
  ( MonadIO    m
  , MonadRaise m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => m Env
read = YAML.readFile =<< absPath

readFrom ::
  ( MonadIO    m
  , MonadRaise m
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => FilePath
  -> m Env
readFrom appPath = YAML.readFile $ appPath </> relPath

absPath :: MonadIO m => m FilePath
absPath = do
  pwd <- getCurrentDirectory
  return $ pwd </> relPath

relPath :: FilePath
relPath = "fission.yaml"

ignoreDefault :: [Text]
ignoreDefault =
  [ Text.pack relPath
  , ".env"
  , ".DS_Store"
  ]
