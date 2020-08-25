module Fission.Types
  ( Fission (..)
  , module Fission.Config.Types
  ) where

import           Control.Monad.Catch

import qualified RIO.ByteString.Lazy                   as Lazy
import           RIO.NonEmpty
import qualified RIO.Text                              as Text

import           Database.Esqueleto                    as SQL hiding ((<&>))

import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Network.AWS                           as AWS hiding (Request)
import           Network.AWS.Route53

import qualified Network.IPFS                          as IPFS
import qualified Network.IPFS.Peer                     as Peer
import qualified Network.IPFS.Pin                      as IPFS.Pin
import qualified Network.IPFS.Process                  as IPFS
import qualified Network.IPFS.Process.Error            as IPFS.Process
import qualified Network.IPFS.Stat                     as IPFS.Stat
import qualified Network.IPFS.Types                    as IPFS

import           Fission.Config.Types
import qualified Fission.Internal.UTF8                 as UTF8
import           Fission.Prelude

import           Fission.AWS
import           Fission.AWS.Types                     as AWS
import           Fission.Error                         as Error
import           Fission.Models

import           Fission.DNS                           as DNS
import           Fission.URL                           as URL

import qualified Fission.App                           as App
import qualified Fission.App.Destroyer                 as App.Destroyer
import           Fission.IPFS.DNSLink                  as DNSLink
import           Fission.User.Username.Types

import qualified Fission.Web.Error                     as Web.Error
import           Fission.Web.Types


import           Fission.IPFS.Linked
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import           Fission.Authorization.Types
import           Fission.AWS                           as AWS
import           Fission.AWS.Route53                   as Route53

import           Fission.Platform.Heroku.Types         as Heroku

import           Fission.Web.Auth                      as Auth
import qualified Fission.Web.Auth.DID                  as Auth.DID
import qualified Fission.Web.Auth.Token                as Auth.Token

import           Fission.Web.Handler
import           Fission.Web.Server.Reflective         as Reflective

import qualified Fission.User                          as User
import           Fission.User.Creator.Class
import           Fission.User.DID                      as DID
import qualified Fission.User.Modifier.Class           as User.Modifier
import qualified Fission.User.Password                 as Password

import qualified Fission.Key                           as Key

import           Fission.Web.Auth.Token.Basic.Class
import qualified Fission.Web.Auth.Token.JWT.RawContent as JWT
import           Fission.Web.Auth.Token.JWT.Resolver   as JWT

import           Fission.Authorization.ServerDID.Class

import           Fission.App.Content                   as App.Content
import           Fission.App.Domain                    as App.Domain

import           Fission.Challenge                     as Challenge
import qualified Fission.Email                         as Email
import           Fission.Email.Class

import qualified Fission.Domain                        as Domain

-- | The top-level app type
newtype Fission a = Fission { unFission :: RIO Config a }
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

instance MonadLogger Fission where
  monadLoggerLog loc src lvl msg = Fission $ monadLoggerLog loc src lvl msg

instance MonadTime Fission where
  currentTime = liftIO getCurrentTime

instance MonadReflectiveServer Fission where
  getHost = asks host

instance MonadDB (Transaction Fission) Fission where
  runDB transaction = do
    pool <- asks dbPool
    SQL.runSqlPool transaction pool

instance MonadAWS Fission where
  liftAWS awsAction = do
    accessKey <- asks awsAccessKey
    secretKey <- asks awsSecretKey
    env       <- newEnv $ FromKeys accessKey secretKey

    runResourceT $ runAWS env awsAction

instance MonadRoute53 Fission where
  clear recordType url@URL {..} (ZoneID zoneTxt) = do
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

  set recType url@URL {..} (ZoneID zoneTxt) contents ttl = do
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

instance MonadDNSLink Fission where
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

instance MonadLinkedIPFS Fission where
  getLinkedPeers = pure <$> asks ipfsRemotePeer

instance IPFS.MonadLocalIPFS Fission where
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

instance IPFS.MonadRemoteIPFS Fission where
  runRemote query = do
    peerID       <- asks ipfsRemotePeer
    IPFS.URL url <- asks ipfsURL
    manager      <- asks httpManager
    _            <- Peer.connectRetry peerID 2

    liftIO . runClientM query $ mkClientEnv manager url

instance MonadBasicAuth Heroku.Auth Fission where
  getVerifier = do
    Heroku.ID       hkuID   <- asks herokuID
    Heroku.Password hkuPass <- asks herokuPassword
    return $ Heroku.Auth <$> Auth.basic hkuID hkuPass

instance MonadAuth DID Fission where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unFission $ Auth.DID.handler req

instance MonadAuth Authorization Fission where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unFission $ Auth.Token.handler req

instance App.Domain.Initializer Fission where
  initial = asks baseAppDomain

instance App.Content.Initializer Fission where
  placeholder = asks appPlaceholder

instance JWT.Resolver Fission where
  resolve cid@(IPFS.CID hash) =
    IPFS.runLocal ["cat"] (Lazy.fromStrict $ encodeUtf8 hash) <&> \case
      Left errMsg ->
        Left $ CannotResolve cid errMsg

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (JWT.contentOf (decodeUtf8Lenient resolvedBS), jwt)

instance ServerDID Fission where
  getServerDID = asks fissionDID

instance PublicizeServerDID Fission where
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
            let status = view crrsrsResponseStatus resp
            if status < 300
              then ok
              else Left $ Web.Error.toServerError status

instance User.Retriever Fission where
  getById            userId   = runDB $ User.getById userId
  getByUsername      username = runDB $ User.getByUsername username
  getByPublicKey     pk       = runDB $ User.getByPublicKey pk
  getByHerokuAddOnId hId      = runDB $ User.getByHerokuAddOnId hId
  getByEmail         email    = runDB $ User.getByEmail email

instance User.Creator Fission where
  create username@(Username rawUN) pk email now =
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
                  subdomain  = Just $ Subdomain rawUN
                  url        = URL {..}

                  userPublic = dataURL `WithPath` ["public"]
                  dataURL    = URL
                    { domainName
                    , subdomain  = Just $ Subdomain (rawUN <> ".files")
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

instance User.Modifier Fission where
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

      Just (Entity _ User { userUsername = Username username }) ->
        IPFS.Stat.getSizeRemote newCID >>= \case
          Left err ->
            return $ Error.openLeft err

          Right size -> do
            _ <- runDB $ User.setDataDB userId newCID size now

            userDataDomain <- asks userRootDomain
            zoneID         <- asks userZoneID

            let
              url = URL
                { domainName = userDataDomain
                , subdomain  = Just $ Subdomain (username <> ".files")
                }

            DNSLink.set userId url zoneID newCID >>= \case
              Left err ->
                return $ Error.relaxedLeft err

              Right _  ->
                IPFS.Pin.add newCID >>= \case
                  Right _  -> return ok
                  Left err -> return $ Error.openLeft err

instance User.Destroyer Fission where
  deactivate requestorId userId = runDB $ User.deactivate requestorId userId

instance App.Retriever Fission where
  byId    uId appId = runDB $ App.byId    uId appId
  byURL   uId url   = runDB $ App.byURL   uId url
  ownedBy uId       = runDB $ App.ownedBy uId

instance App.Creator Fission where
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

instance App.Modifier Fission where
  setCID userId url newCID copyFiles now =
    IPFS.Stat.getSizeRemote newCID >>= \case
      Left err ->
        return $ Error.openLeft err

      Right size ->
        runDB (App.setCidDB userId url newCID size copyFiles now) >>= \case
          Left err ->
            return $ Left err

          Right appId -> do
            runDB (App.Domain.primarySibling userId url) >>= \case
              Left err ->
                return $ relaxedLeft err

              Right (Entity _ AppDomain {..}) ->
                Domain.getByDomainName appDomainDomainName >>= \case
                  Left err ->
                    return $ openLeft err

                  Right Domain {domainZoneId} ->
                    DNSLink.set userId (URL appDomainDomainName appDomainSubdomain) domainZoneId newCID >>= \case
                      Left err ->
                        return $ relaxedLeft err

                      Right _ ->
                        if copyFiles
                          then
                            IPFS.Pin.add newCID <&> \case
                              Right _  -> Right appId
                              Left err -> Error.openLeft err

                          else
                            return $ Right appId

instance App.Destroyer Fission where
  destroy uId appId now =
    runDB (App.destroy uId appId now) >>= \case
      Left err   -> return $ Left err
      Right urls -> pullFromDNS urls

  destroyByURL uId domainName maySubdomain now =
    runDB (App.destroyByURL uId domainName maySubdomain now) >>= \case
      Left err   -> return $ Left err
      Right urls -> pullFromDNS urls

instance Heroku.AddOn.Creator Fission where
  create uuid region now = runDB $ Heroku.AddOn.create uuid region now

instance Domain.Retriever Fission where
  getByDomainName domain = runDB $ Domain.getByDomainName domain

instance Domain.Creator Fission where
  create domainName userId zoneId now =
    runDB $ Domain.create domainName userId zoneId now

instance Challenge.Creator Fission where
  create email =
    runDB $ Challenge.create email

instance Challenge.Verifier Fission where
  verify challenge =
    runDB $ Challenge.verify challenge

instance MonadEmail Fission where
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

    liftIO $ runClientM (Email.sendEmail apiKey emailData) env

pullFromDNS :: [URL] -> Fission (Either App.Destroyer.Errors' [URL])
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
      -> Fission (Either App.Destroyer.Errors' [URL])

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
     Transaction Fission (Either User.Modifier.Errors' a)
  -> (a -> Text)
  -> UserId
  -> Text
  -> Fission (Either User.Modifier.Errors' a)
runUserUpdate updateDB dbValToTxt uID subdomain =
  runDB updateDB >>= \case
    Left err ->
      return $ Left err

    Right dbVal -> do
      runDB (User.getById uID) >>= \case
        Nothing ->
          return . Error.openLeft $ NotFound @User

        Just (Entity _ User { userUsername = Username rawUN }) -> do
          domainName <- asks userRootDomain
          zoneID     <- asks userZoneID

          let
            url = URL {domainName, subdomain = Just (Subdomain subdomain) <> Just (Subdomain rawUN)}
            segments = DNS.splitRecord $ dbValToTxt dbVal

          Route53.set Txt url zoneID segments 10 >>= \case
            Left serverErr -> return $ Error.openLeft serverErr
            Right _        -> return $ Right dbVal
