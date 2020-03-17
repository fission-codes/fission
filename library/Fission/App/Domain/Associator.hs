module Fission.App.Domain.Associator
  ( module Fission.App.Domain.Associator.Class
  , associateDefault
  ) where

import           Fission.Prelude
import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App.Domain.Initializer as AppDomain

-- Re-export

import           Fission.App.Domain.Associator.Class

associateDefault ::
  ( MonadIO               m
  , Associator            m
  , AppDomain.Initializer m
  )
  => UserId
  -> AppId
  -> UTCTime
  -> m (Either Errors Subdomain)
associateDefault userId appId now = do
  defaultDomainName <- AppDomain.initial
  subdomain         <- liftIO (generate arbitrary)

  associate userId appId defaultDomainName (Just subdomain) now
    <&> fmap \_ -> subdomain
