module Fission.CLI.Parser.Command.App.Delegate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Delegate.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.App.Delegate.Types
import           Fission.CLI.Parser.Quiet.Types

import           Fission.Internal.UTF8

import           Fission.Web.Auth.Token.UCAN.Potency.Types      as Potency

import           Web.DID.Types as DID

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Delegate capability to an audience DID"
    ]

parser :: Parser Options
parser = do
  appName <- strOption $ mconcat
    [ help    "The target app"
    -----------
    , long    "app-name"
    , short   'a'
    -----------
    , metavar "NAME"
    ]

  audienceDid <- option did $ mconcat
    [ help    "An audience DID"
    -----------
    , long    "did"
    , short   'd'
    -----------
    , metavar "DID"
    ]

  potency <- option parseAbility $ mconcat
    [ help    "The potency to delegate. Options include Append, Destroy, or Super_User."
    , showDefaultWith $ \_ -> show @Text "Append"
    -----------
    , long    "potency"
    , short   'p'
    -----------
    , metavar "POTENCY"
    -----------
    , value $ Right AppendOnly
    ]

  lifetimeInSeconds <- option auto $ mconcat
    [ help    "Lifetime in seconds before UCAN expires"
    , showDefault
    -----------
    , long    "lifetime"
    , short   'l'
    -----------
    , metavar "LIFETIME"
    -----------
    , value (300 :: Int)
    ]

  quiet <- fmap QuietFlag . switch $ mconcat
    [ help  "Only output the UCAN on success"
    ----------
    , long  "quiet"
    , short 'q'
    ]

  return Options {..}

did :: ReadM (Either String DID)
did = eitherDecodeStrict' . wrapIn "\"" <$> str

parseAbility :: ReadM (Either String Potency)
parseAbility = eitherDecodeStrict' . wrapIn "\"" <$> str