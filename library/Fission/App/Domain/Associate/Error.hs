module Fission.App.Domain.Associate.Error (AlreadyExists (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Web.Error.Class
import qualified Fission.Internal.UTF8 as UTF8

data AlreadyExists
  = AlreadyExists AppId DomainName (Maybe Subdomain)
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError AlreadyExists where
  toServerError (AlreadyExists _ (DomainName domainName) maySubdomain) =
    err409 { errBody = "An app already exists at " <> UTF8.textToLazyBS url }
    where
      url = case maySubdomain of
        Nothing              -> domainName
        Just (Subdomain sub) -> sub <> "." <> domainName
