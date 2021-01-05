module Fission.Web.Server.Handler.User.VerifyEmail (handler) where

import           Servant

import           Fission.Prelude

import           Fission.Web.Server.Challenge.Types
import qualified Fission.Web.Server.Challenge.Verifier.Class as Challenge

import           Fission.Web.Server.Error                    as Web.Err
import           Fission.Web.Server.Redirect

hanlder :: (MonadThrow m, MonadLogger m, Challenge.Verifier m) => ServerT API m
handler challenge =
  Challenge.verify challenge >>= \case
    Left _ ->
      Web.Err.throw err404 { errBody = "User does not exist" }

    Right _ ->
      redirect "https://fission.codes?verified=true"
