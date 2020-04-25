module Fission.Types
  ( Fission (..)
  , module Fission.Config.Types
  ) where

import qualified Data.Aeson as JSON

import           Control.Monad.Catch
import qualified Database.Persist.Sql as SQL

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Network.AWS as AWS hiding (Request)
import           Network.AWS.Route53

import           Network.IPFS               as IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process
import qualified Network.IPFS.Peer          as Peer

import           Fission.Prelude
import           Fission.Config.Types

import           Fission.AWS
import           Fission.AWS.Types as AWS

import           Fission.Web.Types
import qualified Fission.Web.Error as Web.Error

import           Fission.IPFS.DNSLink as DNSLink
import           Fission.IPFS.Linked

import           Fission.Authorization.Types
import qualified Fission.URL as URL

import           Fission.Platform.Heroku.Types as Heroku

import           Fission.Web.Auth       as Auth
import qualified Fission.Web.Auth.DID   as Auth.DID
import qualified Fission.Web.Auth.Token as Auth.Token

import           Fission.Web.Server.Reflective as Reflective
import           Fission.Web.Handler

import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.Basic.Class
import           Fission.Web.Auth.Token.JWT.Resolver as JWT
 
import           Fission.Authorization.ServerDID.Class

import           Fission.App.Content as App.Content
import           Fission.App.Domain  as App.Domain

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
  update recordType (URL.DomainName domain) content = do
    AWS.Route53MockEnabled mockRoute53 <- asks awsRoute53MockEnabled

    if mockRoute53
       then changeRecordMock
       else changeRecord'

    where
      changeRecordMock = do
          mockTime <- currentTime

          let
            mockMessage = mconcat
              [ "MOCK: Updating DNS "
              , show recordType
              , " record at: "
              , show domain
              , " with "
              , show content
              ]

            mockId             = "test123"
            mockChangeInfo     = changeInfo mockId Pending mockTime
            mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

          logDebug mockMessage
          return (Right mockRecordResponse)

      changeRecord' = do
        logDebug $ "Updating DNS record at: " <> displayShow domain

        req <- createChangeRequest

        AWS.within NorthVirginia do
          res <- send req
          return $ validate res

      -- | Create the AWS change request for Route53
      createChangeRequest = do
        ZoneID zoneId <- asks awsZoneID
        content
          |> addValue (resourceRecordSet domain recordType)
          |> change Upsert
          |> return
          |> changeBatch
          |> changeResourceRecordSets (ResourceId zoneId)
          |> return

      addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
      addValue recordSet value =
        recordSet
          |> rrsTTL ?~ 10
          |> rrsResourceRecords ?~ pure (resourceRecord value)

instance MonadDNSLink Fission where
  set domain maySubdomain (CID hash) = do
    IPFS.Gateway gateway <- asks ipfsGateway

    let
      baseURL    = URL.normalizePrefix domain maySubdomain
      dnsLinkURL = URL.prefix baseURL (URL.Subdomain "_dnslink")
      dnsLink    = "dnslink=/ipfs/" <> hash

    update Cname baseURL gateway >>= \case
      Left err ->
        return (Left err)

      Right _ ->
        update Txt dnsLinkURL ("\"" <> dnsLink <> "\"")
          <&> \_ -> Right baseURL

  setBase subdomain cid = do
    domain <- asks baseAppDomainName
    DNSLink.set domain (Just subdomain) cid

instance MonadLinkedIPFS Fission where
  getLinkedPeers = pure <$> asks ipfsRemotePeer

instance MonadLocalIPFS Fission where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks ipfsPath
    IPFS.Timeout secs <- asks ipfsTimeout

    let opts' = ("--timeout=" <> show secs <> "s") : opts

    runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' <&> \case
      (ExitSuccess, contents, _) ->
        Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            Left $ Process.Timeout secs
 
        | otherwise ->
            Left $ Process.UnknownErr stdErr

instance MonadRemoteIPFS Fission where
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
  initial = asks baseAppDomainName

instance App.Content.Initializer Fission where
  placeholder = asks appPlaceholder

instance JWT.Resolver Fission where
  resolve cid@(CID hash) =
    IPFS.runLocal ["cat"] (Lazy.fromStrict $ encodeUtf8 hash) <&> \case
      Left errMsg ->
        Left $ CannotResolve cid errMsg

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (decodeUtf8Lenient resolvedBS, jwt)

instance ServerDID Fission where
  getServerDID = asks fissionDID

instance PublicizeServerDID Fission where
  publicize = do
    AWS.Route53MockEnabled mockRoute53 <- asks awsRoute53MockEnabled
 
    Host host <- Reflective.getHost
    did       <- getServerDID

    let
      ourDomain      = URL.DomainName . Text.pack $ baseUrlHost host
      txtRecordURL   = URL.prefix ourDomain $ URL.Subdomain "_did"
      txtRecordValue = decodeUtf8Lenient . Lazy.toStrict $ JSON.encode did

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
        update Txt txtRecordURL txtRecordValue <&> \case
          Left err ->
            Left err

          Right resp ->
            let
              status = view crrsrsResponseStatus resp
            in
              if status < 300
                then ok
                else Left $ Web.Error.toServerError status
