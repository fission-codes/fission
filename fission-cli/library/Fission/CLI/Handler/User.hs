module Fission.CLI.Handler.User (interpret) where

import           Fission.Prelude

import qualified Fission.CLI.Base.Types                      as Base
import           Fission.CLI.Types

import qualified Fission.CLI.Handler                         as Handler

import qualified Fission.CLI.Parser.Command.User.Login.Types as Login
import           Fission.CLI.Parser.Command.User.Types       as User

import           Fission.CLI.Handler.Error.Types             (Errs)

interpret ::
  ( Contains Errs errs
  , Contains errs errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)
  )
  => User.Options
  -> FissionCLI errs Base.Config ()
interpret cmd = do
  logDebug @Text "App interpreter"

  case cmd of
    Login Login.Options {..} -> do
      Handler.login optUsername
      return ()

    WhoAmI _ ->
      Handler.whoami
