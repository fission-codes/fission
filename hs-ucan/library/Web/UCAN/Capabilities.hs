{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Capabilities
  ( Proof(..)
  , capabilities
  , checkProof
  , module Web.UCAN.Capabilities.Error
  ) where

import           Data.Aeson

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
    , originator :: DID
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
capabilities ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  let viaParenthood = attenuation & map \capability ->
        Right ProofParenthood { originator = sender, .. }

  viaDelegation <- concatForM proofs \witnessRef ->
    attempt (resolveToken witnessRef) \witnessResolved ->
      attempt (return (parseToken witnessResolved)) \witness -> do
        attempt (return (checkDelegation witness ucan)) \() -> do
          witnessCapabilities <- capabilities witness
          concatForM attenuation \cap ->
            concatForM witnessCapabilities \case
              Left _ -> return []
              Right proof ->
                let delegationProof = makeDelegationProof cap ucan proof
                in if capability proof `canDelegate` cap then
                  return [Right delegationProof]
                else
                  return []

  return (viaParenthood <> viaDelegation)


checkProof :: (DelegationSemantics cap, Eq cap) => UCAN fct cap -> Proof fct cap -> Bool
checkProof ucan proof =
  checkProofLayer ucan proof
  && case proof of
    ProofDelegation{..} ->
      checkProof witness delegation

    _ -> True



-- ㊙️


-- TODO: Make more efficient, don't traverse twice
concatForM :: Monad m => [a] -> (a -> m [b]) -> m [b]
concatForM ls f = concat <$> mapM f ls


attempt :: Monad m => m (Either a b) -> (b -> m [Either a c]) -> m [Either a c]
attempt action f = do
  action >>= \case
    Right a -> f a
    Left e  -> return [Left e]


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
    originator == UCAN.sender ucanClaims
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
