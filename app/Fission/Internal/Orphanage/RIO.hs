{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import           Fission.Prelude

import           RIO.Orphans ()
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Orphans ()

import           Servant.Client
import           Network.HTTP.Client as HTTP

import           Network.AWS
import qualified Network.AWS.Auth as AWS
import           Network.AWS.Route53

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process
import qualified Network.IPFS.Peer          as Peer

import           Fission.AWS
import           Fission.AWS.Types as AWS

import qualified Fission.Config as Config
import           Fission.Internal.UTF8

import           Fission.IPFS.DNSLink
import qualified Fission.URL as URL
import           Fission.IPFS.Linked

import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku
import           Fission.Platform.Heroku.AddOn

import qualified Fission.Web.Auth              as Auth
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Types             as Web

instance Has Web.Host cfg => MonadReflectiveServer (RIO cfg) where
  getHost = Config.get

instance (Has Heroku.ID cfg, Has Heroku.Password cfg) => MonadHerokuAddOn (RIO cfg) where
  authorize = do
    Heroku.ID       hkuID   <- Config.get
    Heroku.Password hkuPass <- Config.get
    return (Auth.basic hkuID hkuPass)

instance (Has AWS.AccessKey cfg, Has AWS.SecretKey cfg) => MonadAWS (RIO cfg) where
  liftAWS awsAction = do
    accessKey :: AWS.AccessKey <- Config.get
    secretKey :: AWS.SecretKey <- Config.get

    env <- newEnv <| FromKeys accessKey secretKey

    awsAction
      |> runAWS env
      |> runResourceT

instance
  ( Has AWS.AccessKey          cfg
  , Has AWS.SecretKey          cfg
  , Has AWS.ZoneID             cfg
  , Has AWS.Route53MockEnabled cfg
  , HasLogFunc                 cfg
  )
  => MonadRoute53 (RIO cfg) where
  update recordType (URL.DomainName domain) content = do
    AWS.Route53MockEnabled mockRoute53 <- Config.get

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
        ZoneID zoneId <- Config.get
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

instance
  ( Has AWS.AccessKey          cfg
  , Has URL.DomainName         cfg
  , Has AWS.SecretKey          cfg
  , Has AWS.ZoneID             cfg
  , Has AWS.Route53MockEnabled cfg
  , Has IPFS.Gateway           cfg
  , HasLogFunc                 cfg
  )
  => MonadDNSLink (RIO cfg) where
  set maySubdomain (CID hash) = do
    IPFS.Gateway gateway <- Config.get
    domain               <- Config.get

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

instance Has IPFS.Peer cfg => MonadLinkedIPFS (RIO cfg) where
  getLinkedPeers = pure <$> Config.get

instance
  ( HasProcessContext cfg
  , HasLogFunc        cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    runLocal opts arg = do
      IPFS.BinPath ipfs <- Config.get
      IPFS.Timeout secs <- Config.get
      let opts' = ("--timeout=" <> show secs <> "s") : opts

      runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
        (ExitSuccess, contents, _) ->
          return <| Right contents
        (ExitFailure _, _, stdErr)
          | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
              return . Left <| Process.Timeout secs
          | otherwise ->
            return . Left <| Process.UnknownErr stdErr

instance
  ( Has IPFS.URL      cfg
  , Has HTTP.Manager  cfg
  , HasProcessContext cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  , Has IPFS.Peer     cfg
  , HasLogFunc        cfg
  )
  => MonadRemoteIPFS (RIO cfg) where
    runRemote query = do
      peerID       <- Config.get
      IPFS.URL url <- Config.get
      manager      <- Config.get

      _ <- Peer.connectRetry peerID 2

      url
        |> mkClientEnv manager
        |> runClientM query
        |> liftIO

instance MonadTime (RIO cfg) where
  currentTime = liftIO getCurrentTime
