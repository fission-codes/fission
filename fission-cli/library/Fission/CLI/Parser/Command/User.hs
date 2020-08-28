module Fission.CLI.Parser.Command.User
  ( parserWithInfo
  , parser
  , fallback
  -- * Reexports
  , module Fission.CLI.Parser.Command.User.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.User.Register as Register
import           Fission.CLI.Parser.Command.User.Types
import qualified Fission.CLI.Parser.Command.User.WhoAmI   as WhoAmI

parserWithInfo :: ParserInfo Options
parserWithInfo =
  (parser <|> fallback) `info` mconcat
    [ fullDesc
    , header   "User commands (defaults to whoami)"
    , progDesc "Fission account & auth management"
    ]

parser :: Parser Options
parser =
  hsubparser $ mconcat
    [ command "whoami"   $ fmap WhoAmI   WhoAmI.parserWithInfo
    , command "register" $ fmap Register Register.parserWithInfo
    ]

fallback :: Parser Options
fallback = WhoAmI <$> WhoAmI.parser
