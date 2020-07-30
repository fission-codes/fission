module Fission.CLI.Command.App.Init.Types
  ( Options           (..)
  , OptionalFilePath  (..)
  , OptionalSubdomain (..)
  ) where

import qualified RIO.Text                    as Text

import           Fission.Prelude             hiding (Options)
import           Fission.URL.Subdomain.Types

-- | Arguments, flags & switches for the `app init` command
data Options = Options
    { appDir    :: !FilePath
    , buildDir  :: !(Maybe FilePath)
    , subdomain :: !(Maybe Subdomain)
    }

newtype OptionalFilePath = OptionalFilePath
  { mayFilePath :: Maybe FilePath }

instance IsString OptionalFilePath where
  fromString = OptionalFilePath . \case
    ""   -> Nothing
    path -> Just path

newtype OptionalSubdomain = OptionalSubdomain
  { maySubdomain :: Maybe Subdomain}

instance IsString OptionalSubdomain where
  fromString = OptionalSubdomain . \case
    ""        -> Nothing
    subdomain -> Just . Subdomain $ Text.pack subdomain
