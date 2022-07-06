module Fission.Web.Server.App.Modifier
  ( module Fission.Web.Server.App.Modifier.Class
  , addFile
  , setCidDB
  ) where


import           Database.Persist                                   as Persist

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Files                                 as IPFS.Files
import qualified Network.IPFS.Pin                                   as IPFS.Pin
import qualified Network.IPFS.Stat                                  as IPFS.Stat

import           Network.IPFS.Remote.Class (MonadRemoteIPFS)

import qualified RIO.ByteString.Lazy                                as Lazy
import           RIO.Text                                           as Text

import           Fission.Prelude                                    hiding (on)

import           Fission.Error                                      as Error
import           Fission.FileSystem.DirectoryName
import           Fission.FileSystem.FileName
import           Fission.URL

import qualified Fission.Web.Server.App.Domain.Retriever            as App.Domain
import           Fission.Web.Server.App.Modifier.Class
import           Fission.Web.Server.App.Retriever.Class (Retriever)
import qualified Fission.Web.Server.App.Retriever.Class             as App.Retriever
import           Fission.Web.Server.Config.Types
import qualified Fission.Web.Server.Domain                          as Domain
import qualified Fission.Web.Server.Domain.Retriever.Class          as Domain.Retriever
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import qualified Fission.Web.Server.IPFS.DNSLink                    as DNSLink
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Class (MonadDB(..))
import           Fission.Web.Server.MonadDB.Types (Transaction)
import           Fission.Web.Server.Ownership


{-| Adds a file to an app.

Notes:
* Overwrites existing file data
* Uses IPFS MFS as a temporary storage to manipulate the DAG structure

-}
addFile ::
  ( App.Domain.Retriever m
  , Domain.Retriever.Retriever m
  , DNSLink.MonadDNSLink m
  , MonadDB t m
  , MonadLogger m
  , MonadReader Config m
  , MonadRemoteIPFS m
  , MonadTime m
  , Modifier t
  , Retriever m
  )
  => UserId
  -> DirectoryName
  -> FileName
  -> Lazy.ByteString
  -> m (Either Errors' ())
addFile userId appName@(DirectoryName appNameText) fileName rawData = do
  now           <- currentTime
  domainName    <- asks baseAppDomain

  let subdomain = Subdomain appNameText
  let url = URL domainName (Just subdomain)

  mayApp        <- App.Retriever.byURL userId url

  case mayApp of
    Left _ -> do
      return . Error.openLeft $ NotFound @App

    Right (Entity appId app@App {appCid}) ->
      if isOwnedBy userId app then do
        -- Update DAG, DNSLink & DB
        let (DomainName domainNameTxt) = domainName

        appendToDag domainNameTxt appName fileName rawData appCid >>= \case
          Left err ->
            return $ Left err

          Right newCID ->
            IPFS.Stat.getSizeRemote newCID >>= \case
              Left err ->
                return . Error.openLeft $ err

              Right size -> do
                Domain.getByDomainName domainName >>= \case
                  Left err ->
                    return . Error.openLeft $ err

                  Right Domain {domainZoneId} -> do
                    DNSLink.set userId url domainZoneId newCID >>= \case
                      Left err -> return . Error.openLeft $ err
                      Right _  -> do
                        runDB (setCIDDirectly now appId size newCID)
                        return $ Right ()

      else
        return . Error.openLeft $ ActionNotAuthorized @App userId


appendToDag ::
  ( MonadRemoteIPFS m
  , MonadLogger m
  )
  => Text
  -> DirectoryName
  -> FileName
  -> Lazy.ByteString
  -> CID
  -> m (Either Errors' CID)
appendToDag domainName (DirectoryName appName) (FileName fileName) rawData appCid = do
  let appCidText = unaddress appCid

  let tmpDirPath = Text.concat [ "/", domainName, "/", appName, "/" ]
  let distDirPath = Text.concat [ tmpDirPath, appCidText, "/" ]
  let filePath = Text.concat [ distDirPath, "uploads/", fileName ]

  IPFS.Files.cp (Left appCid) (Text.unpack tmpDirPath)
  IPFS.Files.write (Text.unpack filePath) rawData
  IPFS.Files.statCID (Text.unpack distDirPath) >>= \case

    Left err ->
      return . Error.openLeft $ err

    Right newCID -> do
      _ <- IPFS.Pin.add newCID
      IPFS.Files.rm (Text.unpack distDirPath)

      return $ Right newCID


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
