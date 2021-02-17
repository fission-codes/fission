-- | Helper tasks for support
module Fission.Web.Server.Internal.Tasks (deleteByUsername) where

import           Database.Persist.Class
import           Fission.Prelude
import           Fission.User.Username.Types
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Types
import qualified Fission.Web.Server.User     as User

deleteByUsername :: Text -> Server ()
deleteByUsername userNameTxt =
  case mkUsername userNameTxt of
    Left _ ->
      error "Invalid username"

    Right uname -> do
      logDebug $ "👻 Deleting user: " <> textDisplay uname
      User.getByUsername uname >>= \case
        Just (Entity userId _) -> runDB $ deleteCascade userId
        Nothing                -> error "User doesn't exist"
