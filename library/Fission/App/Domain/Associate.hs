module Fission.App.Domain.Associate
  ( module Fission.App.Domain.Associate.Class
  , module Fission.App.Domain.Associate.Error
  , associateDefault
  ) where

import Fission.Prelude
import Fission.Models
import Fission.URL.Types

import Fission.App.Domain.Class as BaseAppDomain

import Fission.App.Domain.Associate.Class
import Fission.App.Domain.Associate.Error

associateDefault ::
  ( MonadIO          m
  , Associate        m
  , HasBaseAppDomain m
  )
  => UserId
  -> AppId
  -> UTCTime
  -> m (Either Errors Subdomain)
associateDefault userId appId now = do
  defaultDomainName <- BaseAppDomain.get
  subdomain         <- liftIO (generate arbitrary)

  associate userId appId defaultDomainName (Just subdomain) now
    <&> fmap \_ -> subdomain
