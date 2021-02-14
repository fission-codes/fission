module Fission.CLI.Handler.Setup (SetupErrs, SetupConstraints, setup) where

import           Data.Type.List

import           Crypto.Cipher.AES                 (AES256)
import           Crypto.Error                      as Crypto

import qualified RIO.ByteString                    as BS

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error        as IPFS.Process

import           Network.DNS                       as DNS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8             as UTF8

import           Fission.Error
import           Fission.Key.Error                 as Key
import           Fission.Key.IV.Error              as IV
import qualified Fission.Key.Symmetric.Types       as Symmetric

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error       as Username

import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Web.Client                as Client
import           Fission.Web.Client.HTTP.Class

import           Fission.CLI.Environment           as Env
import qualified Fission.CLI.Environment.OS        as OS

import qualified Fission.CLI.Display.Success       as Display
import qualified Fission.CLI.IPFS.Executable       as Executable
import           Fission.CLI.Key.Store             as Key

import qualified Fission.CLI.Handler.User.Login    as Login
import qualified Fission.CLI.Handler.User.Login    as User
import qualified Fission.CLI.Handler.User.Register as User

type SetupErrs =
  '[ ClientError
   , DNSError
   , IPFS.Process.Error
   , Key.Error
   , NotFound (Symmetric.Key AES256)
   , CryptoError
   , IV.GenError
   , NotFound CID
   , NotFound DID
   , OS.Unsupported
   , Username.Invalid
   ] ++ Login.Errs

type SetupConstraints m =
  ( MonadIO          m
  , MonadLocalIPFS   m
  , MonadManagedHTTP m
  , MonadWebAuth     m (SecretKey SigningKey)

  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` IPFS.Process.Error
  , m `Raises` Key.Error
  , m `Raises` NotFound (Symmetric.Key AES256)
  , m `Raises` CryptoError
  , m `Raises` IV.GenError
  , m `Raises` NotFound CID
  , m `Raises` NotFound DID
  , m `Raises` OS.Unsupported
  , m `Raises` Username.Invalid

  , Errors m `Contains` Errors m
  , ClientError `IsMember` Errors m

  , Show (OpenUnion (Errors m))

  , User.LoginConstraints m
  )

setup :: forall m .
  SetupConstraints m
  => Maybe OS.Supported
  -> BaseUrl
  -> Maybe Username
  -> Maybe Email
  -> m ()
setup maybeOS fissionURL maybeUsername maybeEmail = do
  Key.create $ Proxy @SigningKey
  Key.create $ Proxy @ExchangeKey

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  -- FIXME FIXME FIXME
  UTF8.putText "👤 If you have an existing account, enter the username. [Enter for new account]: "
  username <- getUsername =<< BS.getLine

  UTF8.putText "Setting default config..."
  Env.init username fissionURL Nothing

  Display.putOk "Done"

  where
    getUsername :: ByteString -> m Username
    getUsername "" = do
      logDebug @Text "Setting up new account"
      User.register maybeUsername maybeEmail

    getUsername usernameBS = do
      uName <- ensure . mkUsername $ decodeUtf8Lenient usernameBS
      -- FIXME User.login
      return uName
