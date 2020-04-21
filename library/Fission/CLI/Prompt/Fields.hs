module Fission.CLI.Prompt.Fields
  ( getRequired
  , getRequiredSecret
  ) where

import qualified Data.ByteString.UTF8  as UTF8
import qualified Data.ByteString.Char8 as BS

import           System.Console.Haskeline

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Prompt             as Prompt
import qualified Fission.CLI.Prompt.Error.Types as Prompt.Error
import qualified Fission.CLI.Display.Error      as CLI.Error

-- | Prompt a user for a value and do not accept an empty value
getRequired :: (MonadIO m, MonadLogger m) => ByteString -> m ByteString
getRequired fieldName =
  let
    prompt             = decodeUtf8Lenient (fieldName <> ": ")
    requiredFieldError = showRequiredError fieldName
  in
    Prompt.reaskWithError prompt hasValueCheck requiredFieldError

-- | Prompt a user for a secret and do not accept an empty value
getRequiredSecret :: (MonadIO m, MonadLogger m) => ByteString -> m ByteString
getRequiredSecret fieldName =
  (fieldName <> ": ")
    |> UTF8.toString
    |> getPassword (Just '•')
    |> runInputT defaultSettings
    |> liftIO
    |> bind \case
      Nothing -> do
        logErrorN "Unable to read password"
        showRequiredError fieldName
        getRequiredSecret fieldName

      Just "" -> do
        showRequiredError fieldName
        getRequiredSecret fieldName

      Just password ->
        return $ BS.pack password

showRequiredError :: (MonadIO m, MonadLogger m) => ByteString -> m ()
showRequiredError fieldName =
  CLI.Error.put Prompt.Error.RequiredField $
    UTF8.textShow fieldName <> " is required"

hasValueCheck :: ByteString -> Bool
hasValueCheck value = BS.length value > 0
