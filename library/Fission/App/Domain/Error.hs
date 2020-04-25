module Fission.App.Domain.Error
  ( NotRegisteredToApp (..)
  , AlreadyAssociated      (..)
  ) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Web.Error.Class

data NotRegisteredToApp
  = NotRegisteredToApp AppId DomainName (Maybe Subdomain)
  deriving (Show, Eq)

data AlreadyAssociated = AlreadyAssociated
  { appId          :: AppId
  , domainName     :: DomainName
  , maybeSubdomain :: Maybe Subdomain
  }
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError AlreadyAssociated where
  toServerError (AlreadyAssociated _ (DomainName domainName) maySubdomain) =
    err409 { errBody = "An app already exists at " <> displayLazyBS url }
    where
      url = case maySubdomain of
        Nothing              -> domainName
        Just (Subdomain sub) -> sub <> "." <> domainName

instance Display AlreadyAssociated where
  display AlreadyAssociated {..} =
    mconcat
      [ maybe "" (\sub -> display sub <> ".") maybeSubdomain
      , display domainName
      , "is already associated to App ID "
      , displayShow appId
      ]
