module Fission.CLI.Handler.UCAN (interpret) where

import qualified Data.Yaml                                        as YAML
import           RIO.Map ((!?))
import           Servant.API

import           Web.DID.Types                                    as DID
import qualified Web.UCAN.Types                                   as UCAN

import           Fission.Prelude

import qualified Fission.Internal.UTF8                            as UTF8
import           Fission.Error.NotFound.Types
import           Fission.Web.Client.Auth.Class

import           Fission.Web.Auth.Token.UCAN
import           Fission.Web.Auth.Token.UCAN.Types
import qualified Fission.Web.Auth.Token.UCAN.Potency.Types        as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Resource.Types       as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Resource.Scope.Types as UCAN
import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer

import           Fission.CLI.Environment                          as Env
import qualified Fission.CLI.WebNative.Mutation.Auth              as WebNative.Mutation.Auth

import qualified Crypto.PubKey.Ed25519 as Ed25519

interpret ::
  ( MonadIO      m
  , MonadTime    m
  , MonadWebAuth m Ed25519.SecretKey
  , WebNative.Mutation.Auth.MonadStore m
  , MonadEnvironment m
  , MonadLogger m

  , MonadRescue  m
  , m `Raises` NotFound UCAN
  , m `Raises` NotFound FilePath
  , m `Raises` YAML.ParseException
  )
  => Maybe (UCAN.Scope UCAN.Resource)
  -> Maybe (UCAN.Potency)
  -> [Fact]
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> DID
  -> m ()
interpret mayResource mayPotency facts mayNbf mayExp receiverDID = do
  now   <- currentTime
  sk    <- getAuth
  proof <-
    attempt Env.get >>= \case
      Left _ -> do
        return UCAN.RootCredential

      Right Env {rootProof} ->
        case rootProof of
          Nothing -> do
            return UCAN.RootCredential

          Just cid -> do
            store <- WebNative.Mutation.Auth.getAll
            case store !? cid of
              Nothing                -> raise  $ NotFound @UCAN
              Just Bearer.Token {..} -> return $ UCAN.Nested rawContent jwt

  let
    begin   = fromMaybe (addUTCTime (secondsToNominalDiffTime (-30)) now) mayNbf
    expiry  = fromMaybe (addUTCTime (secondsToNominalDiffTime   30)  now) mayExp
    ucan    = mkUCAN receiverDID sk begin expiry facts mayResource mayPotency proof

  UTF8.putText $ toUrlPiece ucan
