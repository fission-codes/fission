module Fission.CLI.Parser.Command.UCAN.Generate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.UCAN.Generate.Types
  ) where

import           Options.Applicative
import           Servant.API
import           Web.DID.Types

import           Fission.Prelude

import           Fission.CLI.Parser.Command.UCAN.Generate.Types

import qualified Fission.Web.Auth.Token.UCAN.Fact.Types           as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Potency.Types        as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Resource.Types       as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Resource.Scope.Types as UCAN

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Generate a UCAN"
    ]

parser :: Parser Options
parser = do
  resource <- option (Just <$> resourceM) $ mconcat
    [ help    "UCAN resource"
    ----------
    , value    Nothing
    , metavar "RESOURCE"
    ]

  potency <- option (Just <$> potencyM) $ mconcat
    [ help    "UCAN potency"
    ----------
    , value    Nothing
    , metavar "POTENCY"
    ]

  audience <- option didM $ mconcat
    [ help    "UCAN audience"
    ----------
    , metavar "AUDIENCE"
    ]

  facts <- option factsM $ mconcat
    [ help    "UCAN facts (as string)"
    ----------
    , value    []
    , metavar "Facts"
    ]

  return Options {..}

resourceM :: ReadM (UCAN.Scope UCAN.Resource)
resourceM = do
  txt <- str
  case parseUrlPiece txt of
    Left  err -> fail $ "Unable to parse resource: " <> show err
    Right res -> pure res

potencyM :: ReadM UCAN.Potency
potencyM = do
  txt <- str
  case parseUrlPiece txt of
    Left  err -> fail $ "Unable to parse potency: " <> show err
    Right pot -> pure pot

didM :: ReadM DID
didM = do
  txt <- str
  case eitherDecode txt of
    Left  err -> fail $ "Unable to parse did: " <> show err
    Right did -> pure did

factsM :: ReadM [UCAN.Fact]
factsM = do
  txt <- str
  case eitherDecode txt of
    Left  err -> fail $ "Unable to parse facts: " <> show err
    Right did -> pure did
