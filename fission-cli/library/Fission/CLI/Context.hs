module Fission.CLI.Context (run) where

import           Fission.Prelude

import           Fission.CLI.Types

run ::
  Contains errs errs
  => cfgA
  -> FissionCLI errs cfgA a
  -> FissionCLI errs cfgB a
run cfg = ensureM . runFissionCLI cfg
