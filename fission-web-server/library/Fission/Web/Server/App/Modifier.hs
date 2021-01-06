module Fission.Web.Server.App.Modifier
  ( module Fission.Web.Server.App.Modifier.Class
  , setCidDB
  ) where


import           Database.Persist                                   as Persist

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import           Fission.Prelude                                    hiding (on)

import           Fission.Error                                      as Error
import           Fission.URL

import           Fission.Web.Server.App.Modifier.Class
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Ownership

setCidDB ::
     MonadIO m
  => UserId
  -> URL
  -> CID
  -> Bytes
  -> Bool
  -> UTCTime
  -> Transaction m (Either Errors' AppId)
setCidDB userId URL {..} newCID size _copyFlag now = do
  mayAppDomain <- Persist.selectFirst
    [ AppDomainDomainName ==. domainName
    , AppDomainSubdomain  ==. subdomain
    ] []

  case mayAppDomain of
    Nothing ->
      return . Error.openLeft $ NotFound @AppDomain

    Just (Entity _ AppDomain {appDomainAppId = appId}) ->
      Persist.get appId >>= \case
        Nothing -> do
          return . Error.openLeft $ NotFound @App

        Just app ->
          if isOwnedBy userId app
            then do
              update appId
                [ AppCid  =. newCID
                , AppSize =. size
                ]
              insert $ SetAppCIDEvent appId newCID size now
              return $ Right appId

            else
            return . Error.openLeft $ ActionNotAuthorized @App userId
