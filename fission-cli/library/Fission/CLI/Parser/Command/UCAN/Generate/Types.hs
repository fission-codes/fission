module Fission.CLI.Parser.Command.UCAN.Generate.Types (Options (..)) where

import qualified Data.Aeson as JSON

import           Fission.Prelude

data Options = Options
  { jsonPayload :: JSON.Value
  } deriving (Show, Eq)
