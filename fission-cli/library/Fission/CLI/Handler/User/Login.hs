module Fission.CLI.Handler.User.Login (login) where

import           Crypto.Error                               as Crypto
import qualified Data.ByteString.Base64                     as Base64
import qualified Data.ByteString.Base64.URL                 as Base64.URL
import qualified Fission.Internal.Base64                    as B64
import qualified Fission.Internal.Base64.URL                as B64.URL
import           Fission.Security.EncryptedWith.Types

import qualified Data.ByteString.Char8                      as C8

import           Crypto.Cipher.AES                          (AES256)
import           Crypto.Cipher.Types
import           Crypto.Hash.Algorithms
import           Data.ByteArray                             hiding (all, and,
                                                             length, or)

import qualified Fission.Internal.UTF8                      as UTF8

import qualified RIO.ByteString.Lazy                        as Lazy
import qualified RIO.Text                                   as Text

import           Crypto.Cipher.AES                          (AES256)
import qualified Crypto.PubKey.RSA.OAEP                     as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                    as RSA
import           Crypto.Random

import           Fission.CLI.Environment.Class

import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Error.Types

import           Fission.Authorization.Potency.Types

import           Fission.User.DID.NameService.Class         as DID
import           Fission.User.DID.Types
import           Fission.User.Username.Types

import           Fission.CLI.Key.Store.Types
import qualified Fission.User.Username.Types                as User
import qualified Fission.Web.Client.User                    as User

import qualified Fission.CLI.Key.Store                      as Key.Store
import           Fission.Web.Client

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                      as Symmetric

import qualified Fission.WNFS.Access                        as WNFS

import qualified Fission.WNFS.Access.Mutation.Store.Class   as WNFS.Mutation
import qualified Fission.WNFS.Access.Query.Store.Class      as WNFS.Query

import           Fission.Web.Auth.Token.Bearer.Types        as Bearer
import           Fission.Web.Auth.Token.JWT                 as JWT
import qualified Fission.Web.Auth.Token.JWT                 as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error           as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class  as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error  as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation      as UCAN
import qualified Fission.Web.Auth.Token.UCAN                as UCAN

-- import           Fission.Error.AlreadyExists.Types

import           Fission.PubSub                             as PubSub
import           Fission.PubSub.Secure                      as PubSub.Secure

import qualified Fission.CLI.Linking.PIN                    as PIN
import           Fission.CLI.Linking.Types

import qualified Crypto.PubKey.Ed25519                      as Ed25519
import           Fission.Authorization.ServerDID
import           Fission.CLI.Display.Success                as CLI.Success
import           Fission.Web.Auth.Token                     as Auth

import           Fission.Key.Symmetric.AES256.Payload.Types

type AESPayload m expected = SecurePayload m (Symmetric.Key AES256) expected
type RSAPayload m expected = SecurePayload m RSA.PrivateKey expected

login ::
  ( MonadLogger       m
  , MonadIO           m
  , MonadTime         m
  , JWT.Resolver      m
  , ServerDID m
  , WNFS.Mutation.Store m
  , WNFS.Query.Store    m
  , MonadNameService    m
  , MonadPubSubSecure m (Symmetric.Key AES256)
  , MonadPubSubSecure m RSA.PrivateKey

  , MonadNameService  m

  , MonadWebAuth m Auth.Token
  , MonadWebAuth m Ed25519.SecretKey

  , MonadRandom m
  , MonadEnvironment m
  , MonadWebClient m
  , MonadCleanup m
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` CryptoError
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` NotFound DID
  , m `Raises` Key.Store.Error
  , m `Raises` ClientError
  -- , m `Raises` AlreadyExists DID
  , m `Raises` ActionNotAuthorized UCAN.JWT -- FIXME shoudl be more contextual
  , ToJSON   (AESPayload m PIN.PIN) -- FIXME can make cleaner with a constraint alias on pubsubsecure
  , ToJSON   (AESPayload m PINStep) -- FIXME can make cleaner with a constraint alias on pubsubsecure
  , ToJSON   (AESPayload m Text) -- FIXME can make cleaner with a constraint alias on pubsubsecure
  , FromJSON (AESPayload m Bearer.Token)
  , Display (AESPayload m Bearer.Token)
  , FromJSON (AESPayload m LinkData)
  , Display (AESPayload m LinkData)
  , FromJSON (RSAPayload m (Symmetric.Key AES256))
  , Display  (RSAPayload m (Symmetric.Key AES256))
  )
  => Username
  -> m ()
login username = do
  -- FIXME restore check that you're signed in already
  -- attempt (sendRequestM . authClient $ Proxy @User.WhoAmI) >>= \case
  --   Right _ -> raise $ NotFound @DID -- FIXME already logged in
  --   Left  _ -> return ()

  signingSK <- Key.Store.fetch $ Proxy @SigningKey
  signingPK <- Key.Store.toPublic (Proxy @SigningKey) signingSK

  targetDID <- ensureM $ DID.getByUsername username

  let
    myDID   = DID Key (Ed25519PublicKey signingPK)
    topic   = PubSub.Topic $ textDisplay targetDID
    baseURL = BaseUrl Https "runfission.net" 443 "/user/link" -- FIXME check env

  PubSub.connect baseURL topic \conn -> reattempt 10 do
    aesKey <- secureConnection conn () \rsaConn@SecureConnection {key} -> do
      logDebug @Text "Opening RSA pubsub channel"
      reattempt 10 do
        let kickoffDID = DID Key (RSAPublicKey $ RSA.private_pub key)
        broadcastRaw conn kickoffDID -- STEP 2

        reattempt 10 do
          -- STEP 3
          JoinedTransfer {iv, sessionKey, msg} <- listenJSON conn
          let sessionBS = Base64.decodeLenient $ encodeUtf8 sessionKey

          -- FIXME Do this as part of parsing (if possible)
          sessionKeyBS <- ensureM $ RSA.OAEP.decryptSafer oaepParams key sessionBS
          logDebug $ "Encrypted message: " <> msg
          let sessionKeyActual = Symmetric.Key sessionKeyBS
          payload <- ensure $ Symmetric.decrypt sessionKeyActual iv (EncryptedPayload $ Lazy.fromStrict $ Base64.decodeLenient $ encodeUtf8 msg)
          logDebug $ "Decrypted message: " <> payload


          bearerToken :: Bearer.Token <- ensure $ eitherDecodeStrict ("\"Bearer " <> payload <> "\"") -- FIXME change JSON parser to not use withText

          -- NOTE @expede -- It's been about a week, but I believe that these are taken care of above
          -- aesKey      <- secureListenJSON rsaConn
          -- bearerToken <- secureListenJSON SecureConnection {conn, key = aesKey}-- sessionKey

          -- STEP 4
          validateProof bearerToken targetDID kickoffDID sessionKeyActual
          return sessionKeyActual

    let aesConn = SecureConnection {conn, key = aesKey}

    -- Step 5
    iv <- Symmetric.genIV >>= \case
      Nothing -> raise "NOPE bad IV"
      Just iv -> return iv

    -- pin@(PIN.PIN pTXT) <- PIN.create
    -- logDebug $ "PIN TEXT: " <> pTXT

    -- EncryptedPayload payloadLBS :: (PIN.PIN `EncryptedWith` AES256) <- ensure $ Symmetric.encrypt aesKey iv pin
    -- broadcastRaw conn (payloadLBS)

    a <- liftIO $ generate arbitrary
    b <- liftIO $ generate arbitrary
    c <- liftIO $ generate arbitrary
    d <- liftIO $ generate arbitrary
    e <- liftIO $ generate arbitrary
    f <- liftIO $ generate arbitrary

    let pin = PINStep myDID (a, b, c, d, e, f) -- FIXME

    UTF8.putTextLn $ "Confirmation code: " <> textDisplay pin
    secureBroadcastJSON aesConn pin

    -- STEP 6
    reattempt 100 do
      -- ld@LinkData {..} <- secureListenJSON aesConn
      Payload  {secretMessage, iv} <- listenJSON conn
      msgBS <- ensure $ Symmetric.decrypt aesKey iv (EncryptedPayload $ Lazy.fromStrict $ Base64.decodeLenient $ Lazy.toStrict $ cipherLBS secretMessage)
      LinkData {..} <- ensure $ eitherDecodeStrict msgBS
      logDebug $ show ucanRaw
      -- FIXME getting invalid signature from FE; confirmed with JWT.io
      -- ensureM $ UCAN.check myDID ucanRaw ucanJWT
      localUCAN@UCAN.JWT {claims = UCAN.Claims {sender}, sig} <- ensureM $ UCAN.getRoot ucanJWT
      logDebug $ show localUCAN

      if sender == targetDID
        then WNFS.login username myDID readKey ucanRaw sig
        else raise "unauthorized" -- FIXME

data JoinedTransfer = JoinedTransfer
  { sessionKey :: !Text -- (Symmetric.Key AES256)
  , iv         :: !(IV AES256)
  , msg        :: !Text
  }

instance FromJSON JoinedTransfer where
  parseJSON = withObject "JoinedTransfer" \obj -> do
    sessionKey <- obj .: "sessionKey"
    msg        <- obj .: "msg"
    ivTxt      <- obj .: "iv"

    case makeIV . Base64.decodeLenient $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return JoinedTransfer {..}

oaepParams ::
  ( ByteArray       output
  , ByteArrayAccess seed
  )
  => RSA.OAEP.OAEPParams SHA256 seed output
oaepParams = RSA.OAEP.defaultOAEPParams SHA256

validateProof ::
  ( MonadIO      m
  , MonadTime    m
  , MonadLogger m
  , JWT.Resolver m
  , MonadRaise   m
  , m `Raises` JWT.Error
  , m `Raises` String -- FIXME better error
  )
  => Bearer.Token
  -> DID
  -> DID
  -> Symmetric.Key AES256
  -> m UCAN.JWT
validateProof token@Bearer.Token {..} targetDID myDID sessionAES = do
  logDebug $ show myDID
  logDebug @Text "%%%%%%%%%%%%%%%%"
  logDebug $ show targetDID
  logDebug $ show token
  _ <- ensureM $ UCAN.check myDID rawContent jwt

  -- FIXME waiting on FE to not send an append UCAN -- case (jwt |> claims |> potency) == AuthNOnly of
  case True of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True ->
      case (jwt |> claims |> facts) of
        (SessionKey aesFact : _) ->
          if aesFact == sessionAES
            then return jwt
            else raise "Sesison key doesn't match! ABORT!"

        _ ->
          raise "No session key fact" -- FIXME

newtype Throwaway = Throwaway DID
  deriving newtype (Eq, Show, Display)

instance ToJSON Throwaway where
  toJSON (Throwaway did) = Null -- String "TEMPORARY_EXCHANGE_KEY" --  object ["didThrowaway" .= did]


data PINStep = PINStep
  { did :: !DID
  , pin :: !(Digit, Digit, Digit, Digit, Digit, Digit)
  }
  deriving (Show)

newtype Digit = Digit Natural
  deriving newtype (Show, Eq, Ord, Display, ToJSON, FromJSON)

instance Arbitrary Digit where
  arbitrary = Digit <$> elements [0..9]

instance Display PINStep where
  display PINStep {..} = "PINStep{" <> display did <> ", " <>  "}"

instance ToJSON PINStep where
  toJSON PINStep {did, pin} =
    object [ "did" .= did
           , "pin" .= pin
           ]
