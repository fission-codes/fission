module Fission.CLI.Handler.Setup where

import           Fission.Prelude

import qualified Fission.CLI.IPFS.Executable as Executable

setup maybeOS = do
  -- FIXME actually maybe just do this if the executable is not found
  Executable.place
