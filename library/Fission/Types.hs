module Fission.Types
  ( Fission (..)
  , module Fission.Config.Types
  ) where

import           Control.Monad.Catch
import qualified Database.Persist.Sql as SQL

import qualified RIO.ByteString.Lazy as Lazy

import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Network.AWS as AWS hiding (Request)
import           Network.AWS.Route53

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process
import qualified Network.IPFS.Peer          as Peer

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

import           Fission.Web.Auth.Class
import qualified Fission.Web.Auth     as Auth
import qualified Fission.Web.Auth.DID as Auth.DID

import           Fission.Web.Server.Reflective
import           Fission.Web.Handler

import           Fission.User.DID.Types
import           Fission.Models

import           Fission.Web.Auth.Token.Basic.Class

import Fission.App.Creator.Class
import qualified Fission.App.Domain.Associate.Class as AppDomain
import qualified Fission.App.Domain.Associate.Error as AppDomain
import           Fission.IPFS.DNSLink as DNSLink
import qualified Fission.Error as Error

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

instance MonadDB (Transaction Fission) Fission where
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

        AWS.within NorthVirginia do
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

instance MonadBasicAuth Heroku.Auth Fission where
  getVerifier = do
    Heroku.ID       hkuID   <- asks herokuID
    Heroku.Password hkuPass <- asks herokuPassword

    hkuPass
      |> Auth.basic hkuID
      |> fmap Heroku.Auth
      |> return

instance MonadAuth DID Fission where
  getVerifier = do
    cfg <- ask
    return <| mkAuthHandler \req ->
      toHandler (runRIO cfg) <| unwrapFission <| Auth.DID.handler req

instance MonadAuth (SQL.Entity User) Fission where
  getVerifier = do
    cfg <- ask
    return <| mkAuthHandler \req ->
      toHandler (runRIO cfg) <| unwrapFission <| Auth.handler req

-- instance Creator Fission where
--   create ownerId cid now = do
--     appId <- runDB SQL.insert App
--       { appOwnerId    = ownerId
--       , appCid        = cid
--       , appInsertedAt = now
--       , appModifiedAt = now
--       }

--     AppDomain.associateDefault appId now >>= \case
--       Left err -> return <| Left <| relaxOpenUnion err

--       Right subdomain ->
--         DNSLink.set (Just subdomain) cid >>= return . \case
--           Left  err -> Error.openLeft err
--           Right _   -> Right (appId, subdomain)
