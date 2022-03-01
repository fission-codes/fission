{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Capabilities
  ( Proof(..)
  , capabilities
  , capabilitiesStream
  , checkProof
  , originator
  , module Web.UCAN.Capabilities.Error
  ) where

import           Data.Aeson

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


data Proof fct cap
  = ProofParenthood
    { capability :: cap
    , expiration :: UTCTime
    , notBefore  :: Maybe UTCTime
    , orig       :: DID
    }
  | ProofDelegation
    { capability :: cap
    , expiration :: UTCTime
    , notBefore  :: Maybe UTCTime
    , witness    :: UCAN fct cap
    , delegation :: Proof fct cap
    }
  --  | ProofRightsAmplification
  --   { capability    :: cap
  --   , expiration    :: UTCTime
  --   , notBefore     :: Maybe UTCTime
  --   , amplifiedFrom :: NonEmpty (Proof fct cap, UCAN fct cap)
  --   }
  deriving (Show, Eq)


capabilities ::
  ( DelegationSemantics cap
  , Eq cap
  , FromJSON fct
  , FromJSON cap
  , Resolver m
  )
  => UCAN fct cap
  -> m [Either Error (Proof fct cap)]
capabilities =
  -- toReverseList because order doesn't really matter
  -- and this way we get it in the order they're produced
  ListT.toReverseList . capabilitiesStream


capabilitiesStream ::
  forall cap fct m.
  ( DelegationSemantics cap
  , Eq cap
  , FromJSON fct
  , FromJSON cap
  , Resolver m
  )
  => UCAN fct cap
  -> ListT m (Either Error (Proof fct cap))
capabilitiesStream ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  let
    viaParenthood :: ListT m (Either Error (Proof fct cap))
    viaParenthood = do
      capability <- ListT.fromFoldable attenuation
      return $ Right ProofParenthood { orig = sender, .. }

  viaParenthood <|> do
    witnessRef <- ListT.fromFoldable proofs
    liftAttempt (resolveToken witnessRef) \witnessResolved ->
      attempt (parseToken witnessResolved) \witness -> do
        attempt (checkDelegation witness ucan) \() -> do
          cap <- ListT.fromFoldable attenuation
          -- v0.8.1:
          -- If cap is something like prf:x, thent get prf:x and essentially replace cap with each of the caps in the prf(s)
          -- If cap is something like as:<did>:* then check that proofs also contain either as:<did>:* or my:* and the issuer matches
          -- If cap is anything else, just do the normal canDelegate thing.
          capabilitiesStream witness >>= \case
            Left e -> return $ Left e
            Right proof -> do
              let delegationProof = makeDelegationProof cap ucan proof
              guard $ capability proof `canDelegate` cap
              return $ Right delegationProof


checkProof :: (DelegationSemantics cap, Eq cap) => UCAN fct cap -> Proof fct cap -> Bool
checkProof ucan proof =
  checkProofLayer ucan proof
  && case proof of
    ProofDelegation{..} ->
      checkProof witness delegation

    _ -> True


originator :: Proof fct cap -> DID
originator = \case
  ProofParenthood{..} -> orig
  ProofDelegation{..} -> originator delegation


-- ㊙️


-- | Either return the error from the action as a single element in the stream
--   or take that element and return another stream of values
liftAttempt :: Monad m => m (Either e a) -> (a -> ListT m (Either e b)) -> ListT m (Either e b)
liftAttempt action f =
  lift action >>= \case
    Right a -> f a
    Left e  -> return (Left e)


attempt :: Monad m => Either e a -> (a -> ListT m (Either e b)) -> ListT m (Either e b)
attempt failable f =
  case failable of
    Right a -> f a
    Left e  -> return (Left e)


resolveToken :: Resolver m => UCAN.Witness -> m (Either Error Text)
resolveToken = \case
  UCAN.Nested text -> return $ Right text
  UCAN.Reference cid -> do
    resolved <- resolve cid
    case resolved of
      Left err -> return $ Left $ ResolveError err
      Right bs -> return $ Right $ decodeUtf8Lenient bs


parseToken :: (FromJSON fct, FromJSON cap) => Text -> Either Error (UCAN fct cap)
parseToken token =
  case fromJSON $ String token of
    Error e       -> Left $ ParseError e
    Success proof -> Right proof


checkProofLayer :: (DelegationSemantics cap, Eq cap) => UCAN fct cap -> Proof fct cap -> Bool
checkProofLayer UCAN.UCAN{ claims = ucanClaims } = \case
  ProofParenthood{..} ->
    -- parenthood
    orig == UCAN.sender ucanClaims
    -- Capability integrity
    && any (== capability) (UCAN.attenuation ucanClaims)
    -- Time bounds
    && expiration <= UCAN.expiration ucanClaims
    && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)

  ProofDelegation{ witness = UCAN.UCAN{ claims = witnessClaims }, .. } ->
    -- Proof integrity
         UCAN.sender ucanClaims
    == UCAN.receiver witnessClaims
    -- Capability integrity
    && any (== capability) (UCAN.attenuation ucanClaims)
    -- Delegation ability
    && any (`canDelegate` capability) (UCAN.attenuation witnessClaims)
    -- Time bounds
    && expiration <= UCAN.expiration ucanClaims
    && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)


checkDelegation :: UCAN fct cap -> UCAN fct cap -> Either Error ()
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


makeDelegationProof :: cap -> UCAN fct cap -> Proof fct cap -> Proof fct cap
makeDelegationProof capability ucan delegation =
  ProofDelegation
    { capability = capability
    , expiration = min (UCAN.expiration (UCAN.claims ucan)) (expiration delegation)
    , notBefore = min <$> UCAN.notBefore (UCAN.claims ucan) <*> notBefore delegation
    , witness = ucan
    , delegation = delegation
    }
