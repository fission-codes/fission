module Fission.Authorization.Requestor.Class where

class MonadRequestor m where
  getRequestor :: m (Either Heroku DID)
