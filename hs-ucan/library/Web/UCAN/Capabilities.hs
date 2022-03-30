{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Capabilities
  ( Proof(..)
  , ChainStep(..)
  , ProofAuthentication(..)
  , capabilities
  , capabilitiesStream
  , checkProof
  , originator
  , module Web.UCAN.Capabilities.Error
  ) where

import           Data.Aeson

import           Control.Applicative
import           ListT                       (ListT)
import qualified ListT
import           RIO                         hiding (exp, to)
import           RIO.Time

import           Web.DID.Types
import           Web.UCAN.Resolver.Class
import           Web.UCAN.Types              (UCAN)
import qualified Web.UCAN.Types              as UCAN
import           Web.UCAN.Witness.Class
-- Re-exports

import           Web.UCAN.Capabilities.Error


data ChainStep a
  = ProofByParenthood
  | ProofByDelegation a
  --  | ProofByRightsAmplification (NonEmpty a)
  deriving (Show, Eq, Functor)



data Proof fct res abl
  = ProofAuthorization res (UCAN.Ability abl) (UCAN fct res abl) (ChainStep (Proof fct res abl))
  | ProofAuthentication (ProofAuthentication fct res abl)
  deriving (Show, Eq)


data ProofAuthentication fct res abl
  = ProofAs DID (UCAN.OwnershipScope abl) (UCAN fct res abl) (ProofAuthentication fct res abl)
  | ProofMy (UCAN.OwnershipScope abl) (UCAN fct res abl)
  deriving (Show, Eq)



capabilities ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , FromJSON fct
  , FromJSON res
  , FromJSON abl
  , Resolver m
  )
  => UCAN fct res abl
  -> m [Either Error (Proof fct res abl)]
capabilities =
  -- toReverseList because order doesn't really matter
  -- and this way we get it in the order they're produced
  -- and it's more performant than ListT.toList
  ListT.toReverseList . capabilitiesStream


capabilitiesStream ::
  forall fct res abl m.
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , FromJSON fct
  , FromJSON res
  , FromJSON abl
  , Resolver m
  )
  => UCAN fct res abl
  -> ListT m (Either Error (Proof fct res abl))
capabilitiesStream ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  let
    -- This is how every capability proof tree begins: A capability is always introduced "by parenthood" at some point.
    viaParenthood :: ListT m (Either Error (Proof fct res abl))
    viaParenthood = do
      ListT.fromFoldable attenuation >>= \case
        UCAN.CapResource resource ability ->
          return $ Right $ ProofAuthorization resource ability ucan ProofByParenthood

        -- `Nothing` indicates "My resources". `Just` would indicate re-delegating ownership
        UCAN.CapOwnedResources (UCAN.OwnedResources Nothing ownershipScope) -> do
          return $ Right $ ProofAuthentication $ ProofMy ownershipScope ucan

        _ ->
          empty

  viaParenthood <|> do
    witnessRef <- ListT.fromFoldable proofs
    liftAttempt (lift (resolveToken witnessRef)) \witnessResolved ->
      attempt (parseJSONString witnessResolved) \witness ->
        attempt (checkDelegation witness ucan) \() ->
          ListT.fromFoldable attenuation >>= \case
          -- v0.8.1:
          -- If cap is something like prf:x, thent get prf:x and essentially replace cap with each of the caps in the prf(s)
          -- If cap is something like as:<did>:* then check that proofs also contain either as:<did>:* or my:* and the issuer matches
          -- If cap is anything else, just do the normal canDelegate thing.
            UCAN.CapResource resource ability ->
              liftAttempt (capabilitiesStream witness) \case
                proof@(ProofAuthorization parentResource parentAbility _ _) -> do
                  guard $ (parentResource, parentAbility) `canDelegate` (resource, ability)
                  return $ Right $ ProofAuthorization resource ability ucan $ ProofByDelegation proof

                _ ->
                  empty

            UCAN.CapOwnedResources (UCAN.OwnedResources (Just did) ownershipScope) ->
              liftAttempt (capabilitiesStream witness) \case
                ProofAuthentication parent@(ProofAs parentDid parentOwnershipScope _ _) -> do
                  guard $ parentDid == did
                  guard $ parentOwnershipScope `canDelegate` ownershipScope
                  return $ Right $ ProofAuthentication $ ProofAs did ownershipScope ucan parent

                ProofAuthentication parent@(ProofMy parentOwnershipScope parentUcan) -> do
                  -- in case of my:<scope> the DID is implied to be the UCAN sender
                  guard $ did == UCAN.sender (UCAN.claims parentUcan)
                  guard $ parentOwnershipScope `canDelegate` ownershipScope
                  return $ Right $ ProofAuthentication $ ProofAs did ownershipScope ucan parent

                _ ->
                  empty

            UCAN.CapProofRedelegation UCAN.RedelegateAllProofs -> do
              liftAttempt (capabilitiesStream witness) \case
                proof@(ProofAuthorization resource ability _ _) ->
                  return $ Right $ ProofAuthorization resource ability ucan $ ProofByDelegation proof

                _ ->
                  empty

            _ ->
              empty


checkProof ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  ) => Proof fct res abl -> Bool
checkProof proof =
  True
  -- checkProofLayer proof
  -- && case proof of
  --   ProofAuthorization _ _ _ (ProofByDelegation parentProof) ->
  --     checkProof parentProof

  --   ProofAuthentication authProof ->
  --     checkAuthProof authProof

  --   _ -> True


originator :: Proof fct res abl -> DID
originator (ProofAuthorization _ _ ucan ProofByParenthood)      = UCAN.sender (UCAN.claims ucan)
originator (ProofAuthorization _ _ _ (ProofByDelegation proof)) = originator proof
originator (ProofAuthentication (ProofAs did _ _ _))            = did
originator (ProofAuthentication (ProofMy _ ucan))               = UCAN.sender (UCAN.claims ucan)


-- ㊙️


-- | Either return the error from the action as a single element in the stream
--   or take that element and return another stream of values
liftAttempt :: Monad m => m (Either a t) -> (t -> m (Either a b)) -> m (Either a b)
liftAttempt action f =
  action >>= \case
    Right a -> f a
    Left e  -> return $ Left e


attempt :: Monad m => Either e a -> (a -> ListT m (Either e b)) -> ListT m (Either e b)
attempt failable f =
  case failable of
    Right a -> f a
    Left e  -> return $ Left e


resolveToken :: Resolver m => UCAN.Witness -> m (Either Error Text)
resolveToken = \case
  UCAN.Nested text -> return $ Right text
  UCAN.Reference cid -> do
    resolved <- resolve cid
    case resolved of
      Left err -> return $ Left $ ResolveError err
      Right bs -> return $ Right $ decodeUtf8Lenient bs


parseJSONString ::
  ( FromJSON fct
  , FromJSON res
  , FromJSON abl
  ) => Text -> Either Error (UCAN fct res abl)
parseJSONString token =
  case fromJSON $ String token of
    Error e       -> Left $ ParseError e
    Success proof -> Right proof


-- checkProofLayer ::
--   ( DelegationSemantics res
--   , DelegationSemantics abl
--   , Eq res
--   , Eq abl
--   ) => Proof fct res abl -> Bool
-- checkProofLayer (ProofAuthorization resource ability UCAN.UCAN{ claims } step) =
--   case step of
--     ProofByParenthood ->
--       -- parenthood
--       orig == UCAN.sender ucanClaims
--       -- Capability integrity
--       && any (== capability) (UCAN.attenuation ucanClaims)
--       -- Time bounds
--       && expiration <= UCAN.expiration ucanClaims
--       && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)

--     ProofByDelegation parentProof ->
--       -- Proof integrity
--           UCAN.sender ucanClaims
--       == UCAN.receiver witnessClaims
--       -- Capability integrity
--       && any (== capability) (UCAN.attenuation ucanClaims)
--       -- Delegation ability
--       && checkCapabilityDelegation capability (UCAN.sender ucanClaims) witnessClaims
--       -- Time bounds
--       && expiration <= UCAN.expiration ucanClaims
--       && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)

-- checkAuthProof :: ProofAuthentication fct res abl -> Bool
-- checkAuthProof (ProofAs did scope UCAN.UCAN{ claims } parentProof) =
--   -- Proof integrity
--   UCAN.sender claims


-- checkCapabilityDelegation ::
--   ( DelegationSemantics res
--   , DelegationSemantics abl
--   ) => UCAN.Capability res abl -> DID -> UCAN.Claims fct res abl -> Bool
-- checkCapabilityDelegation capability sender witnessClaims =
--   case capability of
--     UCAN.CapProofRedelegation _ ->
--       True

--     UCAN.CapOwnedResources ownedRes@(UCAN.OwnedResources did _) ->
--       did == sender || any (`canDelegate` ownedRes) (filterOwnedResources =<< UCAN.attenuation witnessClaims)

--     UCAN.CapResource resource ability ->
--       any (`canDelegate` (resource, ability)) (filterResources =<< UCAN.attenuation witnessClaims)
--   where
--     filterOwnedResources = \case
--       UCAN.CapOwnedResources ownedResources -> [ownedResources]
--       _                                     -> []

--     filterResources = \case
--       UCAN.CapResource resource ability -> [(resource, ability)]
--       _                                 -> []


checkDelegation :: UCAN fct res abl -> UCAN fct res abl -> Either Error ()
checkDelegation UCAN.UCAN{ claims = from } UCAN.UCAN{ claims = to } = do
  () <- senderReceiverMatch
  () <- expirationBeforeNotBefore
  () <- notBeforeAfterExpiration
  return ()
  where
    receiver = UCAN.receiver from
    sender = UCAN.sender to

    senderReceiverMatch =
      if sender == receiver then
        Right ()
      else
        Left (DelegationIssuerAudienceMismatch sender receiver)

    expirationBeforeNotBefore =
      case (UCAN.expiration from, UCAN.notBefore to) of
        (exp, Just nbf) | exp <= nbf ->
          Left $ DelegationNotBeforeWitnessExpired nbf exp

        _ ->
          Right ()

    notBeforeAfterExpiration =
      case (UCAN.notBefore from, UCAN.expiration to) of
        (Just nbf, exp) | nbf >= exp ->
          Left $ DelegationExpiresAfterNotBefore exp nbf

        _ ->
          Right ()


