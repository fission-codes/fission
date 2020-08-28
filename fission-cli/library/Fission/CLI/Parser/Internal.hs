module Fission.CLI.Parser.Internal (boolean) where

import           Options.Applicative
import qualified RIO.Text            as Text

import           Fission.Prelude

boolean :: ReadM Bool
boolean = do
  txt <- str
  case Text.toLower txt of
    "false" -> pure False
    "f"     -> pure False
    "0"     -> pure False

    "true"  -> pure True
    "t"     -> pure True
    "1"     -> pure True

    _       -> fail "Not a valid boolean"
