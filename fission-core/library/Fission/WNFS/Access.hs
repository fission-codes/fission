module Fission.WNFS.Access
  ( login
  ) where

import           Crypto.Cipher.AES                                (AES256)
import qualified Data.ByteArray                                   as BA


import qualified RIO.ByteString.Lazy                              as Lazy

import           Fission.Prelude

import           Fission.Error.Types

import           Fission.User.DID.Types

import qualified Fission.Key.Symmetric.Types                      as Symmetric

import           Fission.User.DID.NameService.Class               as DID
import           Fission.User.Username.Types

import           Fission.Web.Auth.Token.JWT                       as UCAN
import           Fission.Web.Auth.Token.JWT.Error                 as JWT
import qualified Fission.Web.Auth.Token.JWT.Signature.Types       as UCAN
import qualified Fission.Web.Auth.Token.JWT.Validation            as UCAN
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import qualified Fission.Web.Auth.Token.JWT.Resolver.Class        as UCAN
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error        as UCAN.Resolver

import           Fission.Authorization.Potency.Types
import qualified Fission.Authorization.Potency.Types              as WNFS

import qualified Fission.WNFS.Access.Mutation.Store.Class         as WNFS.Mutation
import qualified Fission.WNFS.Access.Query.Store.Class            as WNFS.Query

import qualified Fission.Web.Auth.Token.UCAN                      as JWT

login ::
  ( WNFS.Mutation.Store m
  , WNFS.Query.Store    m
  , MonadNameService    m
  , UCAN.Resolver       m
  , MonadTime           m
  , MonadLogger         m
  , MonadRaise          m
  , m `Raises` String
  , m `Raises` JWT.Error
  , m `Raises` NotFound DID
  , m `Raises` UCAN.Resolver.Error
  )
  => Username
  -> DID
  -> Symmetric.Key AES256
  -> UCAN.RawContent
  -> UCAN.Signature
  -> m ()
login username machineDID readKey rawUCAN@(UCAN.RawContent ucanTxt) sig = do
  logDebug @Text ">>>>> WRITING UCAN"
  -- FIXME FE sends bad signature at the moment

  -- writeUCAN <- ensure . eitherDecode . Lazy.fromStrict $ encodeUtf8 ucanTxt
  -- JWT {claims = Claims {resource, potency}} <- ensureM $ UCAN.check machineDID rawUCAN writeUCAN

  -- targetDID <- ensureM $ DID.getByUsername username
  -- JWT {claims = Claims {sender}} <- ensureM $ JWT.getRoot writeUCAN

  -- unless (sender == targetDID) $ raise "InvalidUser" -- FIXME! Better error

  -- NOTE This will need to check a BUNCH more on UCAN 0.5
  --  case (resource, potency) of
  --  (Complete, SuperUser) -> do
  WNFS.Mutation.insert username rawUCAN sig
  WNFS.Query.insert    username "/" readKey

  --  _ ->
    --  raise "Bad resource" -- FIXME better error

-- signup username machineDID readKey = do
