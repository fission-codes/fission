-- | Linking requests (asking for a UCAN)
module Fission.CLI.Handler.User.Link.Request (requestRoot) where

import qualified Data.ByteString.Char8                     as BS8

import qualified Network.DNS                               as DNS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import qualified RIO.NonEmpty                              as NonEmpty

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.Key.Asymmetric.Public.Types
import           Fission.Key.Error                         as Key

import           Fission.User.DID.Types
import           Fission.User.Username.Types

import           Fission.CLI.IPFS.Daemon                   as IPFS.Daemon

import qualified Fission.DNS                               as DNS

import qualified Fission.CLI.Display.Error                 as CLI.Error

import qualified Fission.CLI.Linking.Request               as Linking

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types
import qualified Fission.CLI.Key.Store                     as Key.Store
import           Fission.CLI.Key.Store.Types

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import           Fission.User.DID.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import           Fission.CLI.Key.Store                     as KeyStore
import qualified Fission.CLI.Linking.PIN                   as PIN

import           Fission.CLI.Environment.Class

requestRoot ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadKeyStore    m ExchangeKey
  , MonadEnvironment m
  , MonadRescue      m
  , m `Raises` NotFound DID
  , m `Raises` DNS.DNSError
  , m `Raises` Key.Error
  )
  => Username
  -> m ()
requestRoot username = do
  dnsList   <- ensureM $ getDIDByUsername username
  targetDID <- ensureM $ decodeDID dnsList

  sk        <- Key.Store.fetch    (Proxy @SigningKey)
  pk        <- Key.Store.toPublic (Proxy @SigningKey) sk

  undefined -- FIXME Linking.requestFrom targetDID (DID Key $ Ed25519PublicKey pk)

getDIDByUsername ::
  ( MonadIO     m
  , MonadLogger m
  )
  => Username
  -> m (Either DNS.DNSError [ByteString])
getDIDByUsername (Username usernameTxt) = do
  logDebug $ "Fetching DID for " <> usernameTxt

  rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf

  liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
    Left errs -> do
      CLI.Error.put errs ("Unable to find DID for: " <> usernameTxt)
      return $ Left errs

    Right listBS ->
      return $ Right listBS
  where
    url = "_did." <> encodeUtf8 usernameTxt <> ".fissionuser.net" -- FIXME environment

decodeDID ::
  ( MonadIO     m
  , MonadLogger m
  )
  => [ByteString]
  -> m (Either (NotFound DID) DID)
decodeDID listBS = do
  case fmap decodeUtf8Lenient <$> NonEmpty.nonEmpty listBS of
    Nothing -> do
      CLI.Error.put (NotFound @DID) $ "No DID field found"
      return . Left $ NotFound @DID

    Just nonEmptyTxt ->
      case eitherDecode . encode $ DNS.mergeSegments nonEmptyTxt of
        Left errs -> do
          CLI.Error.put errs "Unable to find Fission's ID online"
          return . Left $ NotFound @DID

        Right userDID -> do
          logDebug $ "DID retrieved " <> textDisplay userDID
          return $ Right userDID
