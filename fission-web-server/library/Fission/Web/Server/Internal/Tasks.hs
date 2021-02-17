-- | Helper tasks for support
module Fission.Web.Server.Internal.Tasks (deleteByUsername) where

import Database.Persist.Class
import Fission.Prelude
import Fission.User.Username.Types
import Fission.Web.Server.MonadDB
import Fission.Web.Server.Types
import qualified Fission.Web.Server.User as User

deleteByUsername :: Text -> Server ()
deleteByUsername userNameTxt = do
  case mkUsername userNameTxt of
    Left _ -> error "invalid username"
    Right uname -> do
      logDebug $ "Got username: " <> textDisplay uname
      user <- User.getByUsername uname
      case user of
        Just (Entity userId _) ->
          runDB $ deleteCascade userId
        Nothing -> error "user doesn't exist"
