module Fission.Web.Server.Types
  ( runServer
  , Server (..)
  , module Fission.Web.Server.Config.Types
  ) where

import           Control.Monad.Catch

import qualified RIO.ByteString.Lazy                       as Lazy
import           RIO.NonEmpty                              as NonEmpty
import qualified RIO.Text                                  as Text

import           Database.Esqueleto                        as SQL hiding ((<&>))

import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Network.AWS                               as AWS hiding
                                                                  (Request,
                                                                   Seconds)
import           Network.AWS.Route53

import qualified Network.IPFS                              as IPFS
import qualified Network.IPFS.Add.Error                    as IPFS.Pin
import qualified Network.IPFS.Process                      as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process
import qualified Network.IPFS.Stat                         as IPFS.Stat
import qualified Network.IPFS.Types                        as IPFS

import           Fission.Prelude

import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Error                             as Error
import qualified Fission.Process                           as Process
import           Fission.Time

import           Fission.Web.Server.AWS
import           Fission.Web.Server.AWS.Types              as AWS
import           Fission.Web.Server.Models

import qualified Fission.Web.Server.DID.Publicize.Class    as Server.DID
import           Fission.Web.Server.Host.Types

import           Fission.DNS                               as DNS
import           Fission.URL                               as URL

import qualified Fission.Web.Server.App                    as App
import qualified Fission.Web.Server.App.Destroyer          as App.Destroyer
import qualified Fission.Web.Server.Error                  as Web.Error
import           Fission.Web.Server.WNFS                   as WNFS

import           Fission.Web.Server.IPFS.DNSLink           as DNSLink
import           Fission.Web.Server.IPFS.Linked

import qualified Fission.Web.Server.Heroku.AddOn.Creator   as Heroku.AddOn
import           Fission.Web.Server.Heroku.Types           as Heroku

import           Fission.Web.Server.AWS                    as AWS
import           Fission.Web.Server.AWS.Route53            as Route53
import           Fission.Web.Server.Authorization.Types

import           Fission.Web.Server.Auth                   as Auth
import qualified Fission.Web.Server.Auth.DID               as Auth.DID
import qualified Fission.Web.Server.Auth.Token             as Auth.Token

import           Fission.Web.Server.Handler
import           Fission.Web.Server.Reflective             as Reflective

import           Fission.User.DID                          as DID
import qualified Fission.Web.Server.User                   as User
import           Fission.Web.Server.User.Creator.Class
import qualified Fission.Web.Server.User.Modifier.Class    as User.Modifier
import qualified Fission.Web.Server.User.Password          as Password

import qualified Fission.Key                               as Key

import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Auth.Token.JWT.RawContent     as JWT
import           Fission.Web.Auth.Token.JWT.Resolver       as JWT

import           Fission.Authorization.ServerDID.Class

import           Fission.Web.Server.App.Content            as App.Content
import           Fission.Web.Server.App.Domain             as App.Domain

import           Fission.Web.Server.Challenge              as Challenge
import qualified Fission.Web.Server.Domain                 as Domain
import qualified Fission.Web.Server.Email                  as Email
import           Fission.Web.Server.Email.Class

import           Fission.Web.Server.Auth.Token.Basic.Class
import           Fission.Web.Server.Relay.Store.Class

import           Fission.Web.Server.Config.Types

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

instance MonadRelayStore Server where
  getStoreVar = asks linkRelayStoreVar

instance MonadAWS Server where
  liftAWS awsAction = do
    accessKey <- asks awsAccessKey
    secretKey <- asks awsSecretKey
    env       <- newEnv $ FromKeys accessKey secretKey

    runResourceT $ runAWS env awsAction

instance MonadRoute53 Server where
  clear recordType url (ZoneID zoneTxt) = do
    logDebug $ "Clearing DNS record at: " <> displayShow url
    AWS.MockRoute53 mockRoute53 <- asks awsMockRoute53

    if mockRoute53
      then
        changeRecordMock

      else do
        req <- createChangeRequest zoneTxt

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
      createChangeRequest zoneID = do
        let
          urlTxt = textDisplay url
          fields = resourceRecordSet urlTxt recordType
          batch  = changeBatch . pure $ change Delete fields

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

instance IPFS.MonadLocalIPFS Server where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks ipfsPath
    IPFS.Timeout secs <- asks ipfsTimeout

    let
      opts' = ("--timeout=" <> show secs <> "s") : opts
      args' = byteStringInput arg

    IPFS.runProc readProcess ipfs args' byteStringOutput opts' <&> \case
      (ExitSuccess, contents, _) ->
        Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            Left $ IPFS.Process.Timeout secs

        | otherwise ->
            Left $ IPFS.Process.UnknownErr stdErr

instance IPFS.MonadRemoteIPFS Server where
  runRemote query = do
    clusterURLs       <- asks ipfsURLs
    IPFS.Timeout secs <- asks ipfsTimeout
    manager           <- asks httpManager

    logDebug @Text "Running IPFS request across cluster"
    requests <- forM clusterURLs \(IPFS.URL url) ->
      Process.asyncFor (Seconds (Unity secs)) do
        runClientM query $ mkClientEnv manager url

    liftIO $ untilDone requests

    where
      untilDone ::
           NonEmpty (Async (Either Process.TimedOut (Either ClientError a)))
        -> IO (Either ClientError a)
      untilDone asyncRefs = do
        results <- sequence (getStatus <$> asyncRefs)
        case Process.progress results of
          Process.Failed err -> return $ Left err
          Process.InProgress -> untilDone asyncRefs
          Process.Success a  -> return $ Right a
      -- NOTE letting GHC's scheduler handle the frequency of retries.
      -- This should be pretty lightweight, since we're just waiting for processes to return.
      -- That said, if we go to 99% CPU or something, this is probably a good place to look 😉

      getStatus ::
           Async (Either Process.TimedOut (Either ClientError a))
        -> IO    (Process.Status ClientError a)
      getStatus asyncThread =
        poll asyncThread >>= \case
          Nothing -> -- 🏋️‍ Still working on it
            return Process.InProgress

          Just result ->
            case result of
              Left procError -> -- 🧨 Inner action threw an exception
                return . Process.Failed $ ConnectionError procError

              Right maybeTimedOut ->
                case maybeTimedOut of
                  Left Process.TimedOut -> -- ⏲️  Timed out
                    return . Process.Failed . ConnectionError $ toException Process.TimedOut

                  Right requestResult ->
                    case requestResult of -- 🌐 HTTP request result
                      Left  clientErr -> return $ Process.Failed  clientErr
                      Right a         -> return $ Process.Success a

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

instance App.Content.Initializer Server where
  placeholder = asks appPlaceholder

instance JWT.Resolver Server where
  resolve cid@(IPFS.CID hash) =
    IPFS.runLocal ["cat"] (Lazy.fromStrict $ encodeUtf8 hash) <&> \case
      Left errMsg ->
        Left $ CannotResolve cid errMsg

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (JWT.contentOf (decodeUtf8Lenient resolvedBS), jwt)

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
      txtRecordValue = textDisplay did

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

          Right _ ->
            App.createWithPlaceholder userId Nothing now >>= \case
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
        IPFS.Stat.getSizeRemote newCID >>= \case
          Left err ->
            return $ Error.openLeft err

          Right size -> do
            IPFS.ipfsPin newCID >>= \case
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
          Left err ->
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
    IPFS.Stat.getSizeRemote newCID >>= \case
      Left err ->
        return $ Error.openLeft err

      Right size ->
        runDB (App.Domain.primarySibling userId url) >>= \case
          Left err ->
            return $ relaxedLeft err

          Right (Entity _ AppDomain {..}) ->
            Domain.getByDomainName appDomainDomainName >>= \case
              Left err ->
                return $ openLeft err

              Right Domain {domainZoneId} -> do
                result <- if copyFiles
                            then
                              IPFS.ipfsPin newCID >>= \case
                                Right _  -> return ok
                                Left err -> return . Error.openLeft . IPFS.Pin.IPFSDaemonErr $ textDisplay err

                            else
                              return ok

                case result of
                  Left err ->
                    return $ Left err

                  Right _ ->
                    DNSLink.set userId (URL appDomainDomainName appDomainSubdomain) domainZoneId newCID >>= \case
                      Left err -> return $ relaxedLeft err
                      Right _  -> runDB (App.setCidDB userId url newCID size copyFiles now)

instance App.Destroyer Server where
  destroy uId appId now =
    runDB (App.destroy uId appId now) >>= \case
      Left err   -> return $ Left err
      Right urls -> pullFromDNS urls

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

instance MonadEmail Server where
  sendVerificationEmail recipient@Email.Recipient { name } challenge = do
    httpManager      <- asks tlsManager
    Host baseHostUrl <- asks host
    Host sibUrl      <- asks sibUrl
    apiKey           <- asks sibApiKey
    templateId       <- asks sibTemplateId

    let
      env = mkClientEnv httpManager sibUrl
      path = Text.unpack $ Challenge.verificationLink challenge
      verifyUrl = baseHostUrl { baseUrlPath = path }
      emailData = Email.Request
        { templateId = templateId
        , to = [recipient]
        , params = Email.TemplateOptions verifyUrl name
        }

    mapLeft Email.CouldNotSend <$>
      liftIO (runClientM (Email.sendEmail apiKey emailData) env)

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
          AWS.clear Txt url zoneId <&> \case
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
