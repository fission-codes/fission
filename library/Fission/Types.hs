module Fission.Types
  ( Fission (..)
  , module Fission.Config.Types
  ) where

import           Crypto.BCrypt
import           Control.Monad.Catch
import qualified Database.Persist.Sql as SQL

import qualified RIO.ByteString.Lazy as Lazy

import           Servant
import           Servant.Client

import           Network.AWS
import           Network.AWS.Route53

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process
import qualified Network.IPFS.Peer as Peer

import           Fission.Prelude
import           Fission.Config.Types

import           Fission.AWS
import           Fission.AWS.Types as AWS

import           Fission.Internal.UTF8

import           Fission.IPFS.DNSLink
import           Fission.IPFS.Linked

import qualified Fission.URL as URL

import           Fission.Platform.Heroku.Auth.Types     as Heroku
import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku

import qualified Fission.Web.Auth as Auth
import           Fission.Web.Auth.Class
import           Fission.Web.Server.Reflective

import           Fission.User as User
import           Fission.Models

-- | The top-level app type
newtype Fission a = Fission { unwrapFission :: RIO Config a }
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
  monadLoggerLog loc src lvl msg = Fission (monadLoggerLog loc src lvl msg)

instance MonadTime Fission where
  currentTime = liftIO getCurrentTime

instance MonadReflectiveServer Fission where
  getHost = asks host

instance MonadDB Fission where
  runDB transaction = do
    pool <- asks dbPool
    SQL.runSqlPool transaction pool

instance MonadAWS Fission where
  liftAWS awsAction = do
    accessKey <- asks awsAccessKey
    secretKey <- asks awsSecretKey

    env <- newEnv <| FromKeys accessKey secretKey

    awsAction
      |> runAWS env
      |> runResourceT

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
        logDebug <| "Updating DNS record at: " <> displayShow domain

        req <- createChangeRequest

        within NorthVirginia do
          res <- send req
          return <| validate res

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
  set maySubdomain (CID hash) = do
    IPFS.Gateway gateway <- asks ipfsGateway
    domain               <- asks awsDomainName

    let
      baseURL    = URL.normalizePrefix domain maySubdomain
      dnsLinkURL = URL.prefix baseURL (URL.Subdomain "_dnslink")
      dnsLink    = "dnslink=/ipfs/" <> hash

    update Cname baseURL gateway >>= \case
      Left err ->
        return (Left err)

      Right _ ->
        "\""
          |> wrapIn dnsLink
          |> update Txt dnsLinkURL
          |> fmap \_ -> Right baseURL

instance MonadLinkedIPFS Fission where
  getLinkedPeers = pure <$> asks ipfsRemotePeer

instance MonadLocalIPFS Fission where
    runLocal opts arg = do
      IPFS.BinPath ipfs <- asks ipfsPath
      IPFS.Timeout secs <- asks ipfsTimeout

      let opts' = ("--timeout=" <> show secs <> "s") : opts

      runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
        (ExitSuccess, contents, _) ->
          return <| Right contents

        (ExitFailure _, _, stdErr)
          | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
              return . Left <| Process.Timeout secs
          | otherwise ->
              return . Left <| Process.UnknownErr stdErr

instance MonadRemoteIPFS Fission where
    runRemote query = do
      peerID       <- asks ipfsRemotePeer
      IPFS.URL url <- asks ipfsURL
      manager      <- asks httpManager

      _ <- Peer.connectRetry peerID 2

      url
        |> mkClientEnv manager
        |> runClientM query
        |> liftIO

instance MonadAuth Heroku.Auth Fission where
  verify = do
    Heroku.ID       hkuID   <- asks herokuID
    Heroku.Password hkuPass <- asks herokuPassword

    hkuPass
      |> Auth.basic hkuID
      |> fmap Heroku.Auth
      |> return

instance MonadAuth (SQL.Entity User) Fission where
  verify = do
    cfg <- ask
    return (BasicAuthCheck (check cfg))

    where
      check :: Config -> BasicAuthData -> IO (BasicAuthResult (SQL.Entity User))
      check cfg (BasicAuthData username password) =
        username
          |> decodeUtf8Lenient
          |> User.getByUsername
          |> runDB
          |> bind \case
            Nothing -> do
              logWarn <| attemptMsg username
              return NoSuchUser

            Just usr ->
              validate' usr username password
          |> unwrapFission
          |> runRIO cfg

      validate' :: MonadLogger m => SQL.Entity User -> ByteString -> ByteString -> m (BasicAuthResult (SQL.Entity User))
      validate' usr@(SQL.Entity _ User { userSecretDigest }) username password =
        if validatePassword (encodeUtf8 userSecretDigest) password
           then
              return (Authorized usr)

           else do
             logWarn <| attemptMsg username
             return Unauthorized

      attemptMsg :: ByteString -> ByteString
      attemptMsg username = "Unauthorized user! Attempted with username: " <> username
