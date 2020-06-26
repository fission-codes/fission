module Fission.Web.User.VerifyEmail
  ( API
  , server
  ) where

import           Fission.Prelude
import           Servant

import           Fission.Challenge.Types
import qualified Fission.Challenge.Verifier.Class as Challenge

import           Fission.Web.Error as Web.Err
import           Fission.Web.Redirect

type API
  =  Summary "Email verification"
  :> Description ""
  :> Capture "Challenge" Challenge
  :> Get '[JSON] ()

server :: 
  ( MonadThrow m
  , MonadLogger m
  , Challenge.Verifier m
  )
 => ServerT API m
server challenge =
  Challenge.verify challenge >>= \case
    Left _ ->
      Web.Err.throw err404 { errBody = "User does not exist" }

    Right _ -> 
      redirect "https://fission.codes?verified=true"
