module Fission.CLI.Handler.User.Login (login) where

import           Crypto.Cipher.AES                         (AES256)
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random

import           Fission.CLI.Environment.Class

import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Error.Types

import           Fission.Authorization.Potency.Types

import           Fission.User.DID.NameService.Class        as DID
import           Fission.User.DID.Types
import           Fission.User.Username.Types

import           Fission.CLI.Key.Store.Types
import qualified Fission.User.Username.Types               as User
import qualified Fission.Web.Client.User                   as User

import qualified Fission.CLI.Key.Store                     as Key.Store
import           Fission.Web.Client

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import qualified Fission.WNFS.Access                       as WNFS

import qualified Fission.WNFS.Access.Mutation.Store.Class  as WNFS.Mutation
import qualified Fission.WNFS.Access.Query.Store.Class     as WNFS.Query

import           Fission.Web.Auth.Token.Bearer.Types       as Bearer
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

-- import           Fission.Error.AlreadyExists.Types

import           Fission.PubSub                            as PubSub
import           Fission.PubSub.Secure                     as PubSub.Secure

import qualified Fission.CLI.Linking.PIN                   as PIN
import           Fission.CLI.Linking.Types

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import           Fission.Authorization.ServerDID
import           Fission.CLI.Display.Success               as CLI.Success
import           Fission.Web.Auth.Token                    as Auth

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
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` NotFound DID
  , m `Raises` Key.Store.Error
  , m `Raises` ClientError
  -- , m `Raises` AlreadyExists DID
  , m `Raises` ActionNotAuthorized UCAN.JWT -- FIXME shoudl be more contextual
  , ToJSON   (AESPayload m PIN.PIN) -- FIXME can make cleaner with a constraint alias on pubsubsecure
  , FromJSON (AESPayload m Bearer.Token)
  , FromJSON (AESPayload m LinkData)
  , FromJSON (RSAPayload m (Symmetric.Key AES256))
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
        broadcast conn $ DID Key (RSAPublicKey $ RSA.private_pub key) -- STEP 2

        reattempt 10 do
          -- STEP 3
          aesKey      <- secureListen rsaConn
          bearerToken <- secureListen SecureConnection {conn, key = aesKey}-- sessionKey

          -- STEP 4
          validateProof bearerToken targetDID myDID aesKey
          return aesKey

    let aesConn = SecureConnection {conn, key = aesKey}

    -- Step 5
    pin <- PIN.create
    secureBroadcast aesConn pin

    -- STEP 6
    reattempt 100 do
      LinkData {..} <- secureListen aesConn
      ensureM $ UCAN.check myDID ucanRaw ucanJWT
      UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot ucanJWT

      if sender == targetDID
        then WNFS.login username myDID readKey ucanRaw
        else raise "unauthorized" -- FIXME

validateProof ::
  ( MonadIO      m
  , MonadTime    m
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
validateProof Bearer.Token {..} myDID targetDID sessionAES = do
  _ <- ensureM $ UCAN.check myDID rawContent jwt

  case (jwt |> claims |> potency) == AuthNOnly of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True ->
      case (jwt |> claims |> facts) of
        (SessionKey aesFact : _) ->
          case aesFact == sessionAES of
            False -> raise "Sesison key doesn't match! ABORT!"
            True  -> ensureM $ UCAN.check targetDID rawContent jwt

        _ ->
          raise "No session key fact" -- FIXME
