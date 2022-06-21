module Fission.CLI.Handler.Generate (interpret) where

import           Fission.Prelude

import qualified Fission.CLI.Base.Types                    as Base

import           Fission.CLI.Types

import qualified Fission.CLI.Handler                       as Handler

import           Fission.CLI.Parser.Command.Generate.Types as Generate

import           Fission.CLI.Handler.Error.Types           (Errs)

interpret ::
  ( Contains Errs errs
  , Contains errs errs
  , Display  (OpenUnion errs)
  )
  => Generate.Options
  -> FissionCLI errs Base.Config ()
interpret cmd = do
  logDebug @Text "Generate interpreter"

  case cmd of
    Credentials _ ->
      Handler.credentials
