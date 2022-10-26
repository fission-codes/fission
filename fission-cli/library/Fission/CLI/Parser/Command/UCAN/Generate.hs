module Fission.CLI.Parser.Command.UCAN.Generate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.UCAN.Generate.Types
  ) where

import           Options.Applicative
import qualified Data.Aeson as JSON

import           Fission.Prelude

import           Fission.CLI.Parser.Internal
import           Fission.CLI.Parser.Command.UCAN.Generate.Types

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Generate a UCAN"
    ]

parser :: Parser Options
parser = do
  jsonPayload <- strArgument $ mconcat
    [ help    "The JSON payload"
    ----------
    , metavar "JSON"
    ]

  return Options {..}

json :: ReadM JSON.Value
json = do
  txt <- str
  case JSON.eitherDecodeStrict $ encodeUtf8 txt of
    Left err -> fail $ "Unable to parse JSON: " <> err
    Right j  -> pure j
