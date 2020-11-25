module Fission.CLI.Linking.Provide (listenToLinkRequests) where

import qualified System.Console.ANSI                              as ANSI

import qualified RIO.Text                                         as Text

import           Crypto.Cipher.AES                                (AES256)
import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified Crypto.PubKey.RSA.Types                          as RSA

import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.Internal.Base64.URL                      as B64.URL
import qualified Fission.Internal.UTF8                            as UTF8

import           Fission.Error.NotFound.Types
import qualified Fission.Web.Auth.Token.JWT                       as UCAN

import qualified Fission.Key                                      as Key
import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                            as Symmetric

import           Fission.PubSub                                   as PubSub
import qualified Fission.PubSub.DM.Channel.Types                  as DM
import           Fission.PubSub.Secure                            as PubSub.Secure

import           Fission.Authorization                            as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types           as Key

import           Fission.Challenge.Types
import           Fission.User.DID.Types                           as DID
import           Fission.User.Username.Types

import           Fission.Web.Auth.Token.JWT                       as JWT
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types      as JWT.Typ
import qualified Fission.Web.Auth.Token.JWT.Signature.Types       as JWT.Signature
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import qualified Fission.CLI.Display.Text                         as Display
import           Fission.CLI.Environment.Class
import           Fission.CLI.Key.Store                            as KeyStore
import           Fission.CLI.Linking.Status.Types
import qualified Fission.CLI.Linking.Status.Types                 as Linking
import qualified Fission.CLI.Prompt                               as CLI.Prompt

import           Fission.User.DID.NameService.Class               as DID

import           Fission.WNFS.Access.Mutation.Store               as Mutation

type AESPayload m expected = SecurePayload m (Symmetric.Key AES256) expected
type DMPayload  m expected = SecurePayload m DM.Channel expected
type RSAPayload m expected = SecurePayload m RSA.PrivateKey expected

listenToLinkRequests ::
  ( MonadIO           m
  , MonadTime         m
  , MonadLogger       m
  , MonadKeyStore     m ExchangeKey
  , MonadEnvironment  m
  , MonadNameService  m
  , Mutation.Store    m
  , MonadPubSubSecure m (Symmetric.Key AES256)
  , MonadPubSubSecure m DM.Channel
  , MonadCleanup      m
  , m `Raises` String
  , m `Raises` NotFound UCAN.JWT
  , m `Raises` NotFound DID
  , m `Raises` Error
  , m `Raises` Linking.Status -- FIXME only the Denied makes sense

  , ToJSON (AESPayload m Status)
  , ToJSON (AESPayload m JWT)
  , ToJSON (DMPayload  m (Symmetric.Key AES256))

  , FromJSON (AESPayload m Challenge)
  , FromJSON (AESPayload m DID)
  )
  => Username
  -> m ()
listenToLinkRequests username  = do
  machineSK  <- KeyStore.fetch    (Proxy @SigningKey)
  machinePK  <- KeyStore.toPublic (Proxy @SigningKey) machineSK

  targetDID <- ensureM $ DID.getByUsername username
  proof     <- Mutation.getRootProof username

  let
    machineDID = DID Key (Ed25519PublicKey machinePK)
    topic      = PubSub.Topic $ textDisplay targetDID
    baseURL    = BaseUrl Https "runfission.net" 443 "/user/link"

  PubSub.connect baseURL topic \conn -> reattempt 10 do
    reqDID@(DID Key reqPK) <- listen conn

    reqExchangePK <- case reqPK of
                       RSAPublicKey pk -> return pk
                       _               -> raise "Not an exchange key"

    secureConnection conn reqExchangePK \dmConn@SecureConnection { key = _ :: DM.Channel } -> reattempt 10 do
      secureConnection conn () \aesConn@SecureConnection {key} -> do
        secureBroadcast dmConn key
        now <- currentTime

        let
          sessionProof = authenticateSessionKey reqDID machineSK proof key now

        secureBroadcast aesConn sessionProof

        requestorDID <- reattempt 10 $ secureListen aesConn
        pin          <- reattempt 10 $ secureListen aesConn

        confirmChallenge pin >>= \case
          False -> do
            secureBroadcast aesConn Linking.Denied
            raise Linking.Denied

          True -> do
            let delegatedUCAN = delegateAllTo requestorDID machineSK proof now
            secureBroadcast aesConn delegatedUCAN

confirmChallenge ::
  ( MonadCleanup m
  , MonadLogger m
  , MonadIO m
  )
  => Challenge
  -> m Bool
confirmChallenge (Challenge pinTxt) =
  Display.colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
    UTF8.putText "⚠️⚠️ SECURITY CHECK"

    CLI.Prompt.reaskYN $ Text.intercalate " "
      [ "🔢 Confirm that the following Challenge code is from your other device:"
      , pinTxt
      , "[Y/n]"
      ]

delegateAllTo ::
     DID
  -> SecretKey SigningKey
  -> JWT.Proof
  -> UTCTime
  -> JWT
delegateAllTo toDID fromEdSK proof now = JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith fromEdSK . encodeUtf8 $ B64.URL.encodeJWT header claims

    senderDID = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic fromEdSK
      , method    = DID.Key
      }

    claims = JWT.Claims
      { sender   = senderDID
      , receiver = toDID

      , potency  = SuperUser
      , resource = Complete
      , proof    = proof
      , facts    = []

      , nbf      = secondsToNominalDiffTime (-30)          `addUTCTime` now
      , exp      = secondsToNominalDiffTime  3_000_000_000 `addUTCTime` now
      }

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

authenticateSessionKey ::
     DID
  -> SecretKey SigningKey
  -> JWT.Proof
  -> Symmetric.Key AES256
  -> UTCTime
  -> JWT
authenticateSessionKey toDID fromEdSK proof sessionKey now = JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith fromEdSK . encodeUtf8 $ B64.URL.encodeJWT header claims

    senderDID = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic fromEdSK
      , method    = DID.Key
      }

    claims = JWT.Claims
      { sender   = senderDID
      , receiver = toDID

      , potency  = SuperUser
      , resource = None
      , proof    = proof
      , facts    = [SessionKey sessionKey]

      , nbf      = secondsToNominalDiffTime (-30)          `addUTCTime` now
      , exp      = secondsToNominalDiffTime  3_000_000_000 `addUTCTime` now
      }

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }
