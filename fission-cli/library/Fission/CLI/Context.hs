module Fission.CLI.Context (run) where

import           Fission.Prelude

import           Fission.CLI.Types

run ::
  ( Contains errs errs
  , Display (OpenUnion errs)
  , HasLogFunc cfgB
  )
  => cfgA
  -> FissionCLI errs cfgA a
  -> FissionCLI errs cfgB a
run cfg action = do
  logDebug @Text "FissionCLI context switch"
  ensureM $ runFissionCLI cfg action
