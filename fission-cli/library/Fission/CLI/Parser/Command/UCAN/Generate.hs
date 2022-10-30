module Fission.CLI.Parser.Command.UCAN.Generate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.UCAN.Generate.Types
  ) where

import           Options.Applicative
import qualified RIO.ByteString.Lazy                              as Lazy
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
  mayResource <- option (Just <$> resourceM) $ mconcat
    [ help    "UCAN resource"
    ----------
    , long "resource"
    , short 'r'
    ----------
    , value    Nothing
    , metavar "RESOURCE"
    ]

  mayPotency <- option (Just <$> potencyM) $ mconcat
    [ help    "UCAN potency"
    ----------
    , long "potency"
    , short 'p'
    ----------
    , value    Nothing
    , metavar "POTENCY"
    ]

  audience <- option didM $ mconcat
    [ help    "UCAN audience"
    ----------
    , long "audience"
    , short 'a'
    ----------
    , metavar "AUDIENCE"
    ]

  facts <- option factsM $ mconcat
    [ help    "Facts given as JSON array"
    ----------
    , long "facts"
    , short 'f'
    ----------
    , value    []
    , showDefault
    , metavar "FACTS"
    ]

  mayNbf <- option (Just <$> utcTimeM) $ mconcat
    [ help    "Not before Unix time"
    ----------
    , long "starts"
    , short 'n'
    ----------
    , value    Nothing
    , showDefaultWith $ \_ -> show @Text "30s ago"
    , metavar "NBF"
    ]

  mayExp <- option (Just <$> utcTimeM) $ mconcat
    [ help    "Expiry Unix Time"
    ----------
    , long "expires"
    , short 'e'
    ----------
    , value Nothing
    , showDefaultWith $ \_ -> show @Text "30s from now"
    , metavar "EXP"
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
  case eitherDecode $ encode (txt :: String) of
    Left  err -> fail $ "Unable to parse did: " <> show err
    Right did -> pure did

factsM :: ReadM [UCAN.Fact]
factsM = do
  txt <- str
  case eitherDecode (txt :: Lazy.ByteString) of
    Left  err   -> fail $ "Unable to parse facts: " <> show err
    Right facts -> pure facts

utcTimeM :: ReadM UTCTime
utcTimeM = do
  mayNat <- readMaybe <$> str
  case mayNat of
    Nothing ->
      fail "NaN"

    Just nat ->
      case eitherDecode $ encode (nat :: Natural) of
        Left  err  -> fail $ "Unable to parse UTCTime: " <> show err
        Right secs -> pure $ fromSeconds secs
