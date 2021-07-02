module Fission.Web.Server.Types
  ( runServer
  , Server (..)
  , module Fission.Web.Server.Config.Types
  ) where

import           Control.Monad.Catch                               hiding
                                                                   (finally)
import           Control.Monad.Except

import qualified RIO.ByteString.Lazy                               as Lazy
import           RIO.NonEmpty                                      as NonEmpty
import           RIO.NonEmpty.Partial                              as NonEmpty.Partial
import qualified RIO.Text                                          as Text

import           System.Random                                     as Random

import           Database.Esqueleto                                as SQL hiding
                                                                          ((<&>))

import           Servant.Client
import qualified Servant.Client.Streaming                          as Stream
import           Servant.Server.Experimental.Auth
import qualified Servant.Types.SourceT                             as Stream

import qualified Network.HTTP.Req                                  as HTTP

import           Network.AWS                                       as AWS hiding
                                                                          (Request,
                                                                           Seconds)
import           Network.AWS.Route53

import qualified Network.IPFS                                      as IPFS
import qualified Network.IPFS.Add.Error                            as IPFS.Pin
import           Network.IPFS.Client.Streaming.Pin
import           Network.IPFS.File.Types
import qualified Network.IPFS.Stat                                 as IPFS.Stat
import qualified Network.IPFS.Types                                as IPFS

import           Fission.Prelude

import qualified Fission.Internal.UTF8                             as UTF8

import           Fission.Error                                     as Error
import           Fission.Error.GenericError.Types
import qualified Fission.JSON.Error                                as JSON
import           Fission.Time

import           Fission.Web.Server.AWS.Types                      as AWS
import           Fission.Web.Server.Models

import qualified Fission.Web.Server.DID.Publicize.Class            as Server.DID
import           Fission.Web.Server.Host.Types

import           Fission.DNS                                       as DNS
import           Fission.URL                                       as URL

import           Fission.Web.API.Host.Types
import           Fission.Web.Async

import qualified Fission.Web.Server.App                            as App
import qualified Fission.Web.Server.App.Destroyer                  as App.Destroyer
import qualified Fission.Web.Server.Error                          as Web.Error
import           Fission.Web.Server.WNFS                           as WNFS

import           Fission.Web.Server.IPFS.Cluster                   as Cluster
import           Fission.Web.Server.IPFS.DNSLink                   as DNSLink
import           Fission.Web.Server.IPFS.Linked

import qualified Fission.Web.Server.Heroku.AddOn.Creator           as Heroku.AddOn
import           Fission.Web.Server.Heroku.Types                   as Heroku

import           Fission.Web.Server.AWS                            as AWS
import           Fission.Web.Server.AWS.Route53                    as Route53
import           Fission.Web.Server.Authorization.Types

import           Fission.Web.Server.Auth                           as Auth
import qualified Fission.Web.Server.Auth.DID                       as Auth.DID
import qualified Fission.Web.Server.Auth.Token                     as Auth.Token

import           Fission.Web.Server.Handler
import           Fission.Web.Server.Reflective                     as Reflective

import           Fission.User.DID                                  as DID
import qualified Fission.User.DID.Oldstyle.Types                   as DID

import qualified Fission.Web.Server.User                           as User
import           Fission.Web.Server.User.Creator.Class
import qualified Fission.Web.Server.User.Modifier.Class            as User.Modifier
import qualified Fission.Web.Server.User.Password                  as Password

import qualified Fission.Key                                       as Key

import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Auth.Token.JWT.RawContent             as JWT
import           Fission.Web.Auth.Token.JWT.Resolver               as JWT

import           Fission.Authorization.ServerDID.Class
import qualified Fission.Web.Server.Internal.NGINX.Purge           as NGINX

import           Fission.Web.Server.App.Content                    as App.Content
import           Fission.Web.Server.App.Domain                     as App.Domain

import           Fission.Web.Server.Challenge                      as Challenge
import qualified Fission.Web.Server.Domain                         as Domain
import qualified Fission.Web.Server.Email                          as Email
import           Fission.Web.Server.Email.Class
import           Fission.Web.Server.RecoveryChallenge      as RecoveryChallenge

import           Fission.Web.Server.Auth.Token.Basic.Class
import           Fission.Web.Server.Relay.Store.Class

import           Fission.Web.Server.Config.Types

import           Fission.Web.Server.Internal.Orphanage.ServerError ()

-- | The top-level app type
newtype Server a = Server { unServer :: RIO Config a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader Config
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   )

-- | Run actions described by a @Fission@ type
runServer :: MonadIO m => Config -> Server a -> m a
runServer cfg actions = runRIO cfg $ unServer actions

instance MonadLogger Server where
  monadLoggerLog loc src lvl msg = Server $ monadLoggerLog loc src lvl msg

instance MonadTime Server where
  currentTime = liftIO getCurrentTime

instance MonadReflectiveServer Server where
  getHost = asks host

instance MonadDB (Transaction Server) Server where
  runDB transaction = do
    pool <- asks dbPool
    SQL.runSqlPool transaction pool

instance HTTP.MonadHttp Server where
  -- TODO switch to MonadRaise.raise when that's available in this package
  handleHttpException = \case
    HTTP.VanillaHttpException httpException -> throwM httpException
    HTTP.JsonHttpException txt              ->  throwM $ JSON.Error txt

instance MonadRelayStore Server where
  getStoreVar = asks linkRelayStoreVar

instance MonadAWS Server where
  liftAWS awsAction = do
    accessKey <- asks awsAccessKey
    secretKey <- asks awsSecretKey
    env       <- newEnv $ FromKeys accessKey secretKey

    runResourceT $ runAWS env awsAction

instance MonadRoute53 Server where
  clear url (ZoneID zoneTxt) = do
    logDebug $ "Clearing DNS record at: " <> displayShow url
    AWS.MockRoute53 mockRoute53 <- asks awsMockRoute53

    if mockRoute53
      then
        changeRecordMock

      else do
        Route53.get url (ZoneID zoneTxt) >>= \case
          Left err ->
            return $ Left err

          Right recordSet -> do
            req <- createChangeRequest zoneTxt recordSet

            AWS.within NorthVirginia do
              resp <- send req
              return $ validate resp

    where
      changeRecordMock = do
        mockTime <- currentTime

        let
          mockChangeInfo     = changeInfo "mockId" Pending mockTime
          mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

        return $ Right mockRecordResponse

      -- | Create the AWS change request for Route53
      createChangeRequest zoneID recordSet = do
        let
          batch  = changeBatch . pure $ change Delete recordSet

        return $ changeResourceRecordSets (ResourceId zoneID) batch

  set recType url (ZoneID zoneTxt) contents ttl = do
    logDebug $ "Updating DNS record at: " <> displayShow url
    AWS.MockRoute53 mockRoute53 <- asks awsMockRoute53

    if mockRoute53
      then
        changeRecordMock

      else do
        req <- createChangeRequest zoneTxt

        awsResp <- AWS.within NorthVirginia do
          resp <- send req
          return $ validate resp

        case awsResp of
          Left err -> do
            logWarn @Text "Route53.set failed"
            return $ Left err

          Right good -> do
            logInfo @Text "Route53.set succeeded"
            return $ Right good

    where
      -- | Create the AWS change request for Route53
      createChangeRequest zoneID = do
        let
          urlTxt = textDisplay url
          toSet  = addValues (resourceRecordSet urlTxt recType) contents
          batch  = changeBatch . pure $ change Upsert toSet

        return $ changeResourceRecordSets (ResourceId zoneID) batch

      addValues ::
           ResourceRecordSet
        -> NonEmpty Text
        -> ResourceRecordSet
      addValues recordSet values =
        recordSet
          |> rrsTTL ?~ ttl
          |> rrsResourceRecords ?~ (resourceRecord . format recType <$> values)

      format :: RecordType -> Text -> Text
      format Txt = UTF8.wrapIn "\""
      format _   = identity

      changeRecordMock = do
        mockTime <- currentTime

        let
          mockId             = "test123"
          mockChangeInfo     = changeInfo mockId Pending mockTime
          mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

        return (Right mockRecordResponse)

  get url (ZoneID zoneID) = do
    let
      urlTxt = textShow url
      req = listResourceRecordSets (ResourceId zoneID)
        |> lrrsMaxItems ?~ "1"
        |> lrrsStartRecordName ?~ urlTxt

    awsResp <- AWS.within NorthVirginia do
      resp <- send req
      return $ validate resp

    case awsResp of
      Left err -> return $ Left err

      Right good ->
        case Route53.verifyFirstResource good urlTxt of
          Nothing  -> return . Left $ Web.Error.toServerError (404 :: Int)
          Just rrs -> return $ Right rrs

instance MonadWNFS Server where
  getUserDataRoot username = do
    zoneID <- asks userZoneID
    rootDomain <- asks userRootDomain
    let
      url = URL
        { domainName = rootDomain
        , subdomain  = Just . URL.Subdomain $ "_dnslink." <> textDisplay username <> ".files"
        }

    Route53.get url zoneID >>= \case
      Left err ->
        return $ Left err

      Right rrs ->
        case Route53.getValuesFromRecords rrs of
          Nothing   -> return . Left $ Web.Error.toServerError (404 :: Int)
          Just vals -> return $ Right $ extractCID vals
    where
      extractCID = IPFS.CID . Text.dropPrefix "\"dnslink=/ipfs/" . Text.dropSuffix "\"" . NonEmpty.head

instance MonadDNSLink Server where
  set _userId url@URL {..} zoneID (IPFS.CID hash) = do
    Route53.set Cname url zoneID (pure $ textDisplay gateway) 86400 >>= \case
      Left err ->
        return $ Error.openLeft err

      Right _ ->
        Route53.set Txt dnsLinkURL zoneID (pure dnsLink) 10 <&> \case
          Left err -> Error.openLeft err
          Right _  -> Right url

    where
      gateway    = URL { domainName, subdomain = Just (Subdomain "gateway") }
      dnsLinkURL = URL.prefix' (URL.Subdomain "_dnslink") url
      dnsLink    = "dnslink=/ipfs/" <> hash

  follow _userId url@URL {..} zoneID followeeURL = do
    Route53.set Cname url zoneID (pure $ textDisplay gateway) 86400 >>= \case
      Left err ->
        return $ Error.openLeft err

      Right _ ->
        Route53.set Txt dnsLinkURL zoneID (pure dnsLink) 10 <&> \case
          Left err -> Error.openLeft err
          Right _  -> Right ()

    where
      gateway    = URL { domainName, subdomain = Just (Subdomain "gateway") }
      dnsLinkURL = URL.prefix' (URL.Subdomain "_dnslink") url
      dnsLink    = "dnslink=/ipns/" <> textDisplay followeeURL

instance MonadLinkedIPFS Server where
  getLinkedPeers = asks ipfsRemotePeers

instance IPFS.MonadRemoteIPFS Server where
  runRemote query = do
    manager      <- asks ipfsHttpManager
    remotes      <- asks ipfsURLs
    go manager remotes

    where
      go manager remotes = do
        randomIndex  <- liftIO $ randomRIO (0, NonEmpty.length remotes - 1)
        let
          IPFS.URL url  = remotes NonEmpty.Partial.!! randomIndex -- Partial, but checked above
          hostname      = Text.pack $ baseUrlHost url
          clientManager = mkClientEnv manager url

        logDebug $ "🌌📞 Attempting remote IPFS request with " <> hostname
        liftIO (runClientM query clientManager) >>= \case
          Right val' -> do
            logDebug $ "🌌👍 Remote IPFS node succcess at " <> hostname
            return $ Right val'

          Left err -> do
            case nonEmpty $ NonEmpty.filter (/= IPFS.URL url) remotes of
              Nothing -> do
                logError @Text "🌌🚨 All remote IPFS nodes failed"
                return $ Left err

              Just fewerRemotes -> do
                logWarn $ "🌌⚠️  Remote IPFS node " <> hostname <> " failed: " <> textDisplay err
                go manager fewerRemotes

instance (Eq a, Display a) => MonadIPFSCluster Server a where
  runCluster query = do
    cfg             <- ask
    clusterURLs     <- asks ipfsURLs
    ipfsHttpManager <- asks ipfsHttpManager

    logDebug @Text "🐙🛎️  Running IPFS request across cluster (strict)"
    forM clusterURLs \(IPFS.URL url) ->
      runServer cfg do
        logDebug $ "🐙🎬 Starting request to cluster node: " <> display url
        liftIO . async $ runClientM query (mkClientEnv ipfsHttpManager url)

  streamCluster streamQuery = do
    cfg             <- ask
    clusterURLs     <- asks ipfsURLs
    ipfsHttpManager <- asks ipfsHttpManager

    logDebug @Text "🐙🚰 Running IPFS request across cluster (streaming)"
    forM clusterURLs \(IPFS.URL url) -> do
      resultChan <- liftIO newTChanIO
      latestVar  <- atomically $ newTVar Nothing

      logDebug $ "🐙🎬 Starting request to cluster node: " <> display url

      asyncRef <- liftIO $ async do
        Stream.withClientM streamQuery (mkClientEnv ipfsHttpManager url) \event ->
          case event of
            Left clientErr ->
              runServer cfg do
                logError $ "🐙😭 Cluster node " <> display url <> " reported streaming client error: " <> display clientErr
                return $ Left clientErr

            Right ioSource -> do
              let
                withErr errMsg =
                  runServer cfg do
                    logError $ "🐙😭 Cluster node " <> display url <> " reported generic streaming error: " <> displayShow errMsg
                    let err = ConnectionError . toException $ GenericError errMsg
                    atomically do
                      latestVar  `writeTVar`  Just (Left err)
                      resultChan `writeTChan` Left err

                withVal x =
                  runServer cfg do
                    logDebug $ "🐙📥 Cluster node " <> display url <> " streamed value: " <> display x
                    atomically do
                      latestVar  `writeTVar`  Just (Right x)
                      resultChan `writeTChan` Right x

              Stream.foreach withErr withVal ioSource
              readTVarIO latestVar >>= \case
                Nothing ->
                  runServer cfg do
                    logError $ "🐙🙉 Cluster node " <> display url <> " did not report any streaming updates."
                    return . Left . ConnectionError . toException $ NotFound @PinStatus

                Just finalResult ->
                  runServer cfg do
                    case finalResult of
                      Left err -> do
                        logDebug $ "🐙🚨 Cluster node " <> display url <> " ended stream with an error: " <> display err
                        return $ Left err

                      Right final -> do
                        logDebug $ "🐙👍 Cluster node " <> display url <> " streamed successfully; ended with: " <> display final
                        return $ Right final

      asyncIdleTimeout (Seconds (Unity (120 :: Natural))) asyncRef latestVar
      return (asyncRef, resultChan)

instance MonadBasicAuth Heroku.Auth Server where
  getVerifier = do
    Heroku.ID       hkuID   <- asks herokuID
    Heroku.Password hkuPass <- asks herokuPassword
    return $ Heroku.Auth <$> Auth.basic hkuID hkuPass

instance MonadAuth DID Server where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unServer $ Auth.DID.handler req

instance MonadAuth Authorization Server where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unServer $ Auth.Token.handler req

instance App.Domain.Initializer Server where
  initial = asks baseAppDomain

instance App.Domain.Retriever Server where
  allForOwner userID                   = runDB $ allForOwner userID
  allForApp   appID                    = runDB $ allForApp appID
  allSiblingsByDomain domain subdomain = runDB $ allSiblingsByDomain domain subdomain
  primarySibling userID url            = runDB $ primarySibling userID url

instance App.Content.Initializer Server where
  placeholder = asks appPlaceholder

instance JWT.Resolver Server where
  resolve cid =
    IPFS.ipfsCat cid >>= \case
      Left clientErr ->
        return . Left $ CannotResolve cid clientErr

      Right (Serialized resolvedLBS) ->
        let
          resolvedBS = Lazy.toStrict resolvedLBS
        in
          case eitherDecodeStrict resolvedBS of
            Left  _   -> return . Left $ InvalidJWT resolvedBS
            Right jwt -> return $ Right (JWT.contentOf (decodeUtf8Lenient resolvedBS), jwt)

instance ServerDID Server where
  getServerDID = asks fissionDID

instance Server.DID.Publicize Server where
  publicize = do
    AWS.MockRoute53 mockRoute53 <- asks awsMockRoute53

    Host host <- Reflective.getHost
    did       <- getServerDID
    zoneID    <- asks serverZoneID

    let
      ourURL         = URL (URL.DomainName . Text.pack $ baseUrlHost host) Nothing
      txtRecordURL   = URL.prefix' (URL.Subdomain "_did") ourURL

      txtRecordValue = textDisplay $ DID.Oldstyle did

    if mockRoute53
      then do
        logInfo $ mconcat
          [ "MOCK: Setting DNS setting "
          , textDisplay txtRecordURL
          , " to "
          , txtRecordValue
          ]

        return ok

      else
        Route53.set Txt txtRecordURL zoneID (pure txtRecordValue) 10 <&> \case
          Left err ->
            Left err

          Right resp -> do
            let status' = view crrsrsResponseStatus resp
            if status' < 300
              then ok
              else Left $ Web.Error.toServerError status'

instance User.Retriever Server where
  getById            userId   = runDB $ User.getById userId
  getByUsername      username = runDB $ User.getByUsername username
  getByPublicKey     pk       = runDB $ User.getByPublicKey pk
  getByHerokuAddOnId hId      = runDB $ User.getByHerokuAddOnId hId
  getByEmail         email    = runDB $ User.getByEmail email

instance User.Creator Server where
  create username pk email now =
    runDB (User.createDB username pk email now) >>= \case
      Left err ->
        return $ Left err

      Right userId ->
        User.updatePublicKey userId pk now >>= \case
          Left err ->
            return $ Error.relaxedLeft err

          Right _ -> do
            domainName <- asks userRootDomain
            zoneID     <- asks userZoneID

            let
              subdomain  = Just . Subdomain $ textDisplay username
              url        = URL {..}

              userPublic = dataURL `WithPath` ["public"]
              dataURL    = URL
                { domainName
                , subdomain  = Just $ Subdomain (textDisplay username <> ".files")
                }

            DNSLink.follow userId url zoneID userPublic >>= \case
              Left  err ->
                return $ Error.relaxedLeft err

              Right _ -> do
                defaultCID <- asks defaultDataCID

                User.setData userId defaultCID now <&> \case
                  Left err -> Error.relaxedLeft err
                  Right () -> Right userId

  createWithHeroku herokuUUID herokuRegion username password now =
    runDB $ User.createWithHerokuDB herokuUUID herokuRegion username password now

  createWithPassword username password email now =
    runDB (User.createWithPasswordDB username password email now) >>= \case
      Left err ->
        return $ Left err

      Right userId ->
        App.createWithPlaceholder userId Nothing now <&> \case
          Left err -> Error.relaxedLeft err
          Right _  -> Right userId

instance User.Modifier Server where
  updatePassword uID pass now =
    Password.hashPassword pass >>= \case
      Left err ->
        return $ Left err

      Right secretDigest -> do
        _ <- runDB $ User.updatePasswordDB uID secretDigest now
        return $ Right pass

  updatePublicKey uID pk now =
    runUserUpdate updatePK pkToText uID "_did"
    where
      updatePK = User.updatePublicKeyDB uID pk now
      pkToText pk' = textDisplay (DID Key pk')

  addExchangeKey uID key now =
    runUserUpdate addKey keysToText uID "_exchange"
    where
      addKey = User.addExchangeKeyDB uID key now
      keysToText keys = Text.intercalate "," (textDisplay . DID Key . Key.RSAPublicKey <$> keys)

  removeExchangeKey uID key now =
    runUserUpdate removeKey keysToText uID "_exchange"
    where
      removeKey = User.removeExchangeKeyDB uID key now
      keysToText keys = Text.intercalate "," (textDisplay . DID Key . Key.RSAPublicKey <$> keys)

  setData userId newCID now = do
    runDB (User.getById userId) >>= \case
      Nothing ->
        return . Error.openLeft $ NotFound @User

      Just (Entity _ User { userUsername }) ->
        Cluster.pinStream newCID >>= \case
          Left err ->
            return . Error.openLeft . IPFS.Pin.IPFSDaemonErr $ textDisplay err

          Right _ -> do
            zoneID <- asks userZoneID
            userDataDomain <- asks userRootDomain

            let
              url = URL
                { domainName = userDataDomain
                , subdomain  = Just $ Subdomain (textDisplay userUsername <> ".files")
                }

            IPFS.Stat.getSizeRemote newCID >>= \case
              Left err ->
                return $ Error.openLeft err

              Right size -> do
                DNSLink.set userId url zoneID newCID >>= \case
                  Left err -> return $ Error.relaxedLeft err
                  Right _  -> Right <$> runDB (User.setDataDB userId newCID size now)

instance User.Destroyer Server where
  deactivate requestorId userId = runDB $ User.deactivate requestorId userId

instance App.Retriever Server where
  byId    uId appId = runDB $ App.byId    uId appId
  byURL   uId url   = runDB $ App.byURL   uId url
  ownedBy uId       = runDB $ App.ownedBy uId

instance App.Creator Server where
  create ownerId cid maySubdomain now =
    IPFS.Stat.getSizeRemote cid >>= \case
      Left err ->
        return $ Error.openLeft err

      Right size -> do
        appId <- runDB (App.createDB ownerId cid size now)

        runDB (App.Domain.associateWithFallback ownerId appId maySubdomain now) >>= \case
          Left err -> do
            runDB (App.destroy ownerId appId now)
            return $ Error.relaxedLeft err

          Right subdomain -> do
            appCID     <- App.Content.placeholder
            domainName <- App.Domain.initial
            zoneID     <- asks baseAppZoneID

            let
              url :: URL
              url = URL { domainName, subdomain = Just subdomain }

            DNSLink.set ownerId url zoneID appCID >>= \case
              Left  err -> return $ Error.relaxedLeft err
              Right _   -> return $ Right (appId, subdomain)

instance App.Modifier Server where
  setCID userId url newCID copyFiles now =
    runDB (App.Domain.primarySibling userId url) >>= \case
      Left err ->
        return $ relaxedLeft err

      Right (Entity _ AppDomain {appDomainDomainName = domainName', appDomainSubdomain = subdomain'}) ->
        Domain.getByDomainName domainName' >>= \case
          Left err ->
            return $ openLeft err

          Right Domain {domainZoneId} -> do
            result <- if copyFiles
                        then
                          Cluster.pinStream newCID >>= \case
                            Right _  -> return ok
                            Left err -> return . Error.openLeft . IPFS.Pin.IPFSDaemonErr $ textDisplay err

                        else
                          return ok

            case result of
              Left err ->
                return $ Left err

              Right _ ->
                IPFS.Stat.getSizeRemote newCID >>= \case
                  Left err ->
                    return $ Error.openLeft err

                  Right size ->
                    DNSLink.set userId (URL domainName' subdomain') domainZoneId newCID >>= \case
                      Left err ->
                        return $ relaxedLeft err

                      Right _ -> do
                        runDB (App.setCidDB userId url newCID size copyFiles now) >>= \case
                          Left err ->
                            return $ relaxedLeft err

                          Right appId -> do
                            appDomains <- App.Domain.allForApp appId

                            let
                              urls :: [URL]
                              urls = appDomains <&> \(Entity _ AppDomain {..}) ->
                                        URL { domainName = appDomainDomainName
                                            , subdomain  = appDomainSubdomain
                                            }

                            NGINX.purgeMany urls >>= \case
                              Left err -> return $ openLeft err
                              Right _  -> return $ Right appId

instance App.Destroyer Server where
  destroy uId appId now =
    runDB (App.destroy uId appId now) >>= \case
      Left err   ->
        return $ Left err

      Right urls ->
        pullFromDNS urls >>= \case
          Left errs ->
            return $ Left errs

          Right _ -> do
            NGINX.purgeMany urls >>= \case
              Left errs -> do
                -- Just warn and continue
                logWarn $ textDisplay errs
                return $ Right urls

              Right _ ->
                return $ Right urls

  destroyByURL uId domainName maySubdomain now =
    runDB (App.destroyByURL uId domainName maySubdomain now) >>= \case
      Left err   -> return $ Left err
      Right urls -> pullFromDNS urls

instance Heroku.AddOn.Creator Server where
  create uuid region now = runDB $ Heroku.AddOn.create uuid region now

instance Domain.Retriever Server where
  getByDomainName domain = runDB $ Domain.getByDomainName domain

instance Domain.Creator Server where
  create domainName userId zoneId now =
    runDB $ Domain.create domainName userId zoneId now

instance Challenge.Creator Server where
  create email =
    runDB $ Challenge.create email

instance Challenge.Retriever Server where
  retrieve userId =
    runDB $ Challenge.retrieve userId

instance Challenge.Verifier Server where
  verify challenge =
    runDB $ Challenge.verify challenge

instance RecoveryChallenge.Creator Server where
  create userId now =
    runDB $ RecoveryChallenge.create userId now

instance RecoveryChallenge.Retriever Server where
  retrieve userId now =
    runDB $ RecoveryChallenge.retrieve userId now

instance RecoveryChallenge.Destroyer Server where
  destroyForUser userId =
    runDB $ RecoveryChallenge.destroyForUser userId

instance MonadEmail Server where
  sendVerificationEmail recipient@Email.Recipient { name } challenge = do
    Host baseHostUrl <- asks host
    templateId       <- asks sibVerificationEmailTemplateId

    let
      path = Text.unpack $ Challenge.verificationLink challenge
      verifyUrl = baseHostUrl { baseUrlPath = path }
      email = Email.Request
        { templateId = templateId
        , to = [recipient]
        , params = toJSON $ Email.VerificationTemplateOptions verifyUrl name
        }

    sendSIBEmail email

  sendRecoveryEmail recipient@Email.Recipient { name } challenge = do
    templateId       <- asks sibRecoveryEmailTemplateId
    recoveryAppUrl   <- asks sibRecoveryAppUrl

    let
      recoveryUrl = RecoveryChallenge.recoveryLink recoveryAppUrl challenge
      email = Email.Request
        { templateId = templateId
        , to = [recipient]
        , params = toJSON $ Email.RecoveryTemplateOptions recoveryUrl name
        }

    sendSIBEmail email


sendSIBEmail :: (MonadReader Config m, MonadIO m) => Email.Request -> m (Either Email.CouldNotSend Email.Response)
sendSIBEmail email = do
  httpManager      <- asks tlsManager
  Host sibUrl      <- asks sibUrl
  apiKey           <- asks sibApiKey

  mapLeft Email.CouldNotSend <$>
    liftIO (runClientM (Email.sendEmail apiKey email) (mkClientEnv httpManager sibUrl))


pullFromDNS :: [URL] -> Server (Either App.Destroyer.Errors' [URL])
pullFromDNS urls = do
  domainsAndZoneIDs <- runDB . select $ from \domain -> do
    where_ $ domain ^. DomainDomainName `in_` valList (URL.domainName <$> urls)
    return (domain ^. DomainDomainName, domain ^. DomainZoneId)

  let
    zonesForDomains :: [(DomainName, ZoneID)]
    zonesForDomains =
      domainsAndZoneIDs <&> \(SQL.Value domain, SQL.Value zone) -> (domain, zone)

  foldM (folder zonesForDomains) (Right []) urls

  where
    folder ::
         [(DomainName, ZoneID)]            -- ^ Hosted zone map
      -> Either App.Destroyer.Errors' [URL] -- ^ Accumulator
      -> URL                               -- ^ Focus
      -> Server (Either App.Destroyer.Errors' [URL])

    folder _ (Left err) _ =
      return $ Left err

    folder zonesForDomains (Right accs) url@URL {..} = do
      case lookup domainName zonesForDomains of
        Nothing -> do
          logError $ "Unable to find zone for " <> textDisplay domainName
          return . Error.openLeft $ NotFound @ZoneID

        Just zoneId ->
          AWS.clear url zoneId <&> \case
            Left err -> Error.openLeft err
            Right _  -> Right (url : accs)

runUserUpdate ::
     Transaction Server (Either User.Modifier.Errors' a)
  -> (a -> Text)
  -> UserId
  -> Text
  -> Server (Either User.Modifier.Errors' a)
runUserUpdate updateDB dbValToTxt uID subdomain =
  runDB updateDB >>= \case
    Left err ->
      return $ Left err

    Right dbVal -> do
      runDB (User.getById uID) >>= \case
        Nothing ->
          return . Error.openLeft $ NotFound @User

        Just (Entity _ User { userUsername }) -> do
          domainName <- asks userRootDomain
          zoneID     <- asks userZoneID

          let
            unSubDom = Subdomain $ textDisplay userUsername
            url      = URL {domainName, subdomain = Just (Subdomain subdomain) <> Just unSubDom}
            segments = DNS.splitRecord $ dbValToTxt dbVal

          Route53.set Txt url zoneID segments 10 >>= \case
            Left serverErr -> return $ Error.openLeft serverErr
            Right _        -> return $ Right dbVal
