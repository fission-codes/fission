module Fission.Web.Auth.Token.UCAN
  ( initSession
  , toAuthorization
  -- , getRoot
  ) where

import qualified System.IO.Unsafe                                 as Unsafe

import           Fission.Web.Auth.Token.UCAN.Attenuated.Types
import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import           Fission.Web.Auth.Token.JWT.Resolver              as Proof

import           Fission.Web.Auth.Token.UCAN.Proof.Types


import           Fission.Authorization.Access.Unchecked.Types
import           Fission.Authorization.Session.Types              as Authorization

import qualified Fission.Web.Auth.Token.UCAN.Proof.Types          as Proof

import           Fission.Web.Auth.Token.UCAN.Error                as UCAN


import           Crypto.Hash.Algorithms                           (SHA256 (..))
import           Crypto.Random                                    (MonadRandom (..))

import qualified Crypto.PubKey.RSA                                as RSA
import qualified Crypto.PubKey.RSA.PKCS15                         as RSA.PKCS15
import           Fission.Web.Auth.Token.UCAN.Privilege.Types      as UCAN

import           Crypto.PubKey.Ed25519                            (toPublic)
import qualified Crypto.PubKey.Ed25519                            as Ed25519

import qualified Data.ByteString.Base64.URL                       as BS.B64.URL

import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text

import qualified Fission.Internal.Base64.URL                      as B64.URL
import           Fission.Prelude

import qualified Fission.Key.Asymmetric.Algorithm.Types           as Algorithm

import           Fission.Web.Auth.Token.UCAN.Error

import qualified Fission.Internal.RSA2048.Pair.Types              as RSA2048
import qualified Fission.Internal.UTF8                            as UTF8

import           Fission.Key                                      as Key

import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Header.Types          (Header (..))
import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256

-- Reexports

import           Fission.Web.Auth.Token.JWT.RawContent

-- Orphans

import           Fission.Internal.Orphanage.CID                   ()
import           Fission.Internal.Orphanage.Ed25519.SecretKey     ()


-- import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Prelude

import           Fission.Models

import           Fission.Error.NotFound.Types
import qualified Fission.User.Retriever                           as User
import qualified Fission.Web.Error                                as Web.Error

-- import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Resolver              as Proof
import qualified Fission.Web.Auth.Token.JWT.Validation            as JWT

-- import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver              as JWT

import           Fission.Authorization.ServerDID
-- import qualified Fission.Authorization.Types                      as Authorization
import           Fission.User.DID.Types
import           Fission.User.Username.Types                      (Username)
-- import           Fission.Web.Auth.Token.JWT
-- import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.Web.Auth.Token.UCAN.Types                as UCAN

import qualified Fission.WNFS.Types                               as WNFS

import qualified Fission.Web.Auth.Token.UCAN.Error                as UCAN



import           Fission.Web.Auth.Token.JWT.Resolver.Class        as UCAN

toAuthorization = undefined -- FIXME FIXME FIXME

-- toAuthorization :: -- FIXME probbaly belonmgs in the auth section
--   ( m `Proof.Resolves` UCAN Privilege fact
--   -- , MonadThrow       m
--   -- , MonadLogger      m
--   , MonadDB        t m
--   , User.Retriever t
--   )
--   => UCAN Privilege fact -- FIXME would sure be nice to have a typesafe resolved version
--   -> m (Either JWT.Error Authorization.Session) -- FIXME couldn't hurt to make this more structured
-- toAuthorization ucan@UCAN {claims = UCAN.Claims {..}} = do
--   toUnchecked ucan >>= \case
--     Left  err    -> return $ Left err
--     Right unauths -> return $ Right undefined -- FIXME Auth.Session --  Authorization {sender = Right sender, access}
--
-- toUnchecked ::
--   forall m t privilege fact .
--  ( m `Proof.Resolves` UCAN privilege fact
--   -- , MonadDB        t m
--   , User.Retriever t
--   )
--   => UCAN Privilege fact
--   -> m (Either UCAN.Error [Unchecked Privilege])
-- toUnchecked UCAN {claims = Claims {attenuations = Subset privs}} = do
--   -- Validation walk has to happen here so we can find who granted each permission
--   -- FIXME also is permission better than privilege?
--   let attachGranter = undefined
--   return $ Right (attachGranter <$> privs) -- FIXME switch to lookupGranter GrantedBy
--
-- toUnchecked UCAN {claims = UCAN.Claims {sender = did@(DID {publicKey}), attenuations = AllInScope, proofs}} =
--   runDB case proofs of
--     RootCredential ->
--       User.getByPublicKey publicKey >>= \case
--       --runDB (User.getByPublicKey publicKey) >>= \case
--         Nothing       -> return $ Left UserNotFound (NotFound @User)
--         Just rootUser -> return $ Right [AsUser rootUser]
--
--     DelegatedFrom delegateProofs ->
--       foldM folder (Right []) delegateProofs
--
--   where
--     folder ::
--          Either UCAN.Error [Unchecked privilege]
--       -> DelegateProof (UCAN Privilege fact)
--       -> m (Either UCAN.Error [Unchecked privilege])
--     folder (Left err) _ =
--       return $ Left err
--
--     folder (Right acc) (Nested _ proof) =
--       appendAccesses acc proof
--
--     folder (Right acc) (Reference cid) =
--       UCAN.resolve cid >>= \case
--         Left  err       -> return $ Left err
--         Right (_, ucan) -> appendAccesses acc ucan
--
--     appendAccesses ::
--          [Unchecked privilege]
--       -> UCAN Privilege fact
--       -> [Unchecked privilege]
--     appendAccesses acc ucan =
--       toUnchecked ucan >>= \case
--         Left err       -> return $ Left err
--         Right newPrivs -> return $ Right (acc <> newPrivs)

--------------------
-- KICKOFF SEARCH --
--------------------

initSession ::
  forall t m fact .
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => UCAN Privilege fact
  -> m (Either UCAN.Error Authorization.Session)
initSession ucan@UCAN {claims = UCAN.Claims {sender}} = do
  collectPrivileges ucan >>= \case
    Left err ->
      return $ Left err

    Right unchecked ->
      return $ Right Authorization.Session
        { requestor = Right sender
        , unchecked = unchecked
        , subgraphs = []
        , domains   = []
        , apps      = []
        }

------------------------------------------------------------------
-- EVERYTHING BELOW THIS LINE IS FOR PROOF SEARCH INSIDE A UCAN --
------------------------------------------------------------------

{-

    kickoff <---------+
      |               |
      V               |
    checkNested       |
      |               |
      V               |
    rootOrCntinue-----+


-}

-- FIXME *** STILL NEEDS TO CHECK TIME BOUNDS AND WHATNOT ***

collectAcrossManyDelegateProofs ::
  forall t m fact .
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => NonEmpty (DelegateProof (UCAN Privilege fact))
  -> m (Either UCAN.Error [Unchecked Privilege])
collectAcrossManyDelegateProofs = foldM step (Right [])
  where
    step ::
         Either UCAN.Error [Unchecked Privilege]
      -> DelegateProof (UCAN Privilege fact)
      -> m (Either UCAN.Error [Unchecked Privilege])
    step (Left err) _ =
      return $ Left err

    step (Right acc) delProof =
      case delProof of
        Nested _ nestedUCAN ->
          go acc nestedUCAN

        Reference cid ->
          resolve cid >>= \case
            Left err        -> return . Left $ UCAN.ResolverError err
            Right (_, ucan) -> go acc ucan

    go ::
         [Unchecked Privilege]
      -> UCAN Privilege fact
      -> m (Either UCAN.Error [Unchecked Privilege])
    go acc ucan =
      collectPrivileges ucan >>= \case
        Left  err      -> return $ Left err
        Right newPrivs -> return $ Right (newPrivs <> acc)

collectPrivileges ::
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => UCAN Privilege fact
  -> m (Either UCAN.Error [Unchecked Privilege])
collectPrivileges UCAN {claims = UCAN.Claims {sender = sender@DID {publicKey}, attenuations, proofs}} =
  case attenuations of
    Subset privileges ->
      foldM step (Right []) privileges

    AllInScope ->
      case proofs of
        RootCredential ->
          runDB (User.getByPublicKey publicKey) >>= \case
            Nothing   -> return $ Left UserNotFound
            Just user -> return $ Right [AsUser user]

        DelegatedFrom nested ->
          collectAcrossManyDelegateProofs nested

  where
    step (Left err) _ =
      return $ Left err

    step (Right acc) privilege =
      tracePrivilegeToRoot privilege sender privilege AllInScope proofs >>= \case
        Left  err      -> return $ Left err
        Right newPrivs -> return $ Right (newPrivs <> acc)

tracePrivilegeToRoot ::
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => Privilege
  -> DID
  -> Privilege
  -> Attenuated [Privilege]
  -> Proof (UCAN Privilege fact)
  -> m (Either UCAN.Error [Unchecked Privilege])
tracePrivilegeToRoot initialPriv latestSender focusPriv attenuations proofs =
  case findValidProof focusPriv attenuations of
    Nothing    -> return $ Left NoProof
    Just match -> checkAttenuatedPrivilege initialPriv latestSender focusPriv attenuations proofs -- FIXME hmmmm

-- At the end of this one, we've checked that the focused privilege is backed up by an attenuation in the proof UCAN
checkAttenuatedPrivilege ::
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => Privilege
  -> DID
  -> Privilege
  -> Attenuated [Privilege]
  -> Proof (UCAN Privilege fact)
  -> m (Either UCAN.Error [Unchecked Privilege])
checkAttenuatedPrivilege initialPriv latestDID focusPriv focusAttenuation proof = -- @UCAN {claims = UCAN.Claims {sender, attenuations, proofs}} =
  case focusAttenuation of
    AllInScope ->
      rootOrContinue initialPriv latestDID focusPriv proof

    Subset candidatePrivs ->
      case findValidProof focusPriv (Subset candidatePrivs) of -- FIXME hmmmm
        Nothing       -> return $ Left NoProof
        Just newFocus -> rootOrContinue initialPriv latestDID newFocus proof

-- Here we assume that the attenuations are okay (having already been checked by checkNested)
rootOrContinue ::
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => Privilege
  -> DID
  -> Privilege
  -> Proof (UCAN Privilege fact)
  -> m (Either UCAN.Error [Unchecked Privilege])
rootOrContinue initialPriv DID {publicKey} focusPrivilege = \case
  RootCredential -> -- You've hit root. Yay!
    runDB (User.getByPublicKey publicKey) >>= \case
      Nothing   -> return $ Left UserNotFound
      Just user -> return $ Right [initialPriv `DelegatedBy` user]

  DelegatedFrom delProofs ->
    foldM step (Right []) delProofs

  where
    step (Left err) _ =
      return $ Left err

    step (Right acc) delProof =
      checkInDelegateProof initialPriv focusPrivilege delProof >>= \case
        Left  err      -> return $ Left err
        Right newPrivs -> return $ Right (newPrivs <> acc)

checkInDelegateProof ::
  forall m t fact .
  ( m `Proof.Resolves` UCAN Privilege fact
  , MonadDB t m
  , User.Retriever t
  )
  => Privilege
  -> Privilege
  -> DelegateProof (UCAN Privilege fact)
  -> m (Either UCAN.Error [Unchecked Privilege])
checkInDelegateProof initialPriv focusedPriv = \case
  Nested _ UCAN {claims = UCAN.Claims {sender, attenuations, proofs}} ->
    tracePrivilegeToRoot initialPriv sender focusedPriv attenuations proofs

  Reference cid ->
    resolve cid >>= \case
      Left err ->
        return . Left $ UCAN.ResolverError err

      Right (_, ucan :: UCAN Privilege fact) ->
        case ucan of
          UCAN {claims = UCAN.Claims {sender, attenuations, proofs}} ->
            tracePrivilegeToRoot initialPriv sender focusedPriv attenuations proofs

findValidProof :: Privilege -> Attenuated [Privilege] -> Maybe Privilege
findValidProof priv = \case
  AllInScope ->
    Just priv

  Subset proofPrivs ->
    case filter predicate proofPrivs of
      []          -> Nothing
      (match : _) -> Just match

  where
    predicate a =
      case relationship priv a of
        Equal      -> True
        Descendant -> True
        _          -> False
