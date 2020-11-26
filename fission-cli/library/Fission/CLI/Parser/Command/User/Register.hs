module Fission.CLI.Parser.Command.User.Register
  ( parserWithInfo
  , parser
  -- * Reexport
  , module Fission.CLI.Parser.Command.User.Register.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.User.Username.Types
import           Fission.User.Email.Types
import           Fission.CLI.Parser.Command.User.Register.Types
import qualified Fission.CLI.Parser.Verbose                     as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Set up system"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser

  maybeUsername <- option username $ mconcat

    [ help "The username to register"
    --------
    , long "username"
    , short 'u'
    --------
    , value Nothing
    , metavar "USERNAME"
    ]

  maybeEmail <- option email $ mconcat
    [ help "The email address for the account"
    -------
    , long "email"
    , short 'e'
    --------
    , value Nothing
    , metavar "EMAIL"
    ]

  pure Options {..}

username :: ReadM (Maybe Username)
username = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    name -> Just name

email :: ReadM (Maybe Email)
email = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    mail -> Just mail
