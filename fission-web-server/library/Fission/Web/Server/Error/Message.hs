module Fission.Web.Server.Error.Message (withMessage) where

import           Servant.Server

import           Fission.Prelude

withMessage :: Display err => err -> ServerError -> ServerError
withMessage err statusErr = statusErr { errBody = displayLazyBS err }
