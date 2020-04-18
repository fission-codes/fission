module Fission.CLI.Command.Class (MonadCommand (..)) where

import Fission.Prelude
import Fission.CLI.Command.Types

-- import Fission.SemVer.Types

class Monad m => MonadCommand m where
  add :: Command m input output -> ExceptT output (Writer (Mod CommandFields output)) ()

-- TODO: Need an HList here, so NOPE! Just going to call `add` on each one separately to get it to `m`, incl. the passed monad I guess.

-- -- | Top-level CLI description
-- run :: MonadCommand m
--   -- => SemVer
--   => [forall input . Command m input ()]
--   -> n ()
-- run version runner commands = do
--   (_, runCLI) <- liftIO $
--     simpleOptions version description detail noop (mapM_ runner commands)
--   runCLI
--   where
--     description = "CLI to interact with Fission services"
--     detail      = mconcat [ "Fission makes developing, deploying, updating "
--                           , "and iterating on web applications quick and easy."
--                           ]

runner :: MonadCommand m => Command m input output -> m output
runner cmd = withRunInIO \runIO -> add (runIO cmd)
