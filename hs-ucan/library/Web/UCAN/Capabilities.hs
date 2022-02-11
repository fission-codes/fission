{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Capabilities
  ( Witness(..)
  , capabilities
  , checkWitness
  ) where

import           Data.Aeson

import           RIO

import           RIO.Time
import           Web.DID.Types
import           Web.UCAN.Witness
import           Web.UCAN.Resolver.Class
import           Web.UCAN.Types          (UCAN)
import qualified Web.UCAN.Types          as UCAN


data Witness fct cap
  = WitnessParenthood
    { capability :: cap
    , expiration :: UTCTime
    , notBefore  :: Maybe UTCTime
    , originator :: DID
    }
  | WitnessDelegation
    { capability :: cap
    , expiration :: UTCTime
    , notBefore  :: Maybe UTCTime
    , proof      :: UCAN fct cap
    , delegation :: Witness fct cap
    }
  --  | WitnessRightsAmplification
  --   { capability    :: cap
  --   , expiration    :: UTCTime
  --   , notBefore     :: Maybe UTCTime
  --   , amplifiedFrom :: NonEmpty (Witness fct cap, UCAN fct cap)
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
  -> m [Witness fct cap]
capabilities UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  -- TODO: Consider walking the *proofs first* and then the capabilities.
  -- That way we don't parse proofs N times.
  -- We could then also move proof parsing into this function
  concat <$> forM attenuation \capability ->
    tryDelegating capability proofs >>= \case
      [] ->
        return
          [ WitnessParenthood
            { capability = capability
            , expiration = expiration
            , notBefore  = notBefore
            , originator = sender
            }
          ]

      witnesses ->
        return witnesses


tryDelegating ::
  forall fct cap m .
  ( DelegationSemantics cap
  , Eq cap
  , FromJSON fct
  , FromJSON cap
  , Resolver m
  )
  => cap
  -> [UCAN.Witness]
  -> m [Witness fct cap]
tryDelegating cap fromProofs = do
  candidates <- fromProofs
    & traverse \prf -> do
      resolveToken prf >>= \case
        Nothing -> return [] -- TODO return [Error.FailedToResolve prf]
        Just token ->
          case fromJSON $ String token of
            Error _ -> return [] -- TODO return [Error.FailedToParse token]
            Success proof -> do
              -- TODO return Error.InvalidProof if proof doesn't match.
              caps <- capabilities proof
              return $ makeDelegations proof caps

  return $ concat candidates
  where
    resolveToken :: UCAN.Witness -> m (Maybe Text)
    resolveToken = \case
      UCAN.Nested text -> return $ Just text
      UCAN.Reference cid -> do
        resolved <- resolve cid
        case resolved of
          Left _   -> return Nothing
          Right bs -> return $ Just $ decodeUtf8Lenient bs

    makeDelegationWitness :: UCAN fct cap -> Witness fct cap -> Witness fct cap
    makeDelegationWitness ucan delegation =
      WitnessDelegation
        { capability = cap
        , expiration = min (UCAN.expiration (UCAN.claims ucan)) (expiration delegation)
        , notBefore = min <$> UCAN.notBefore (UCAN.claims ucan) <*> notBefore delegation
        , proof = ucan
        , delegation = delegation
        }

    makeDelegations :: UCAN fct cap -> [Witness fct cap] -> [Witness fct cap]
    makeDelegations ucan =
      filter (checkWitnessIntegrity ucan) . map (makeDelegationWitness ucan)



checkWitnessIntegrity :: (DelegationSemantics cap, Eq cap) => UCAN fct cap -> Witness fct cap -> Bool
checkWitnessIntegrity UCAN.UCAN{ claims = ucanClaims } = \case
  WitnessParenthood{..} ->
    -- parenthood
    originator == UCAN.sender ucanClaims
    -- Capability integrity
    && any (== capability) (UCAN.attenuation ucanClaims)
    -- Time bounds
    && expiration <= UCAN.expiration ucanClaims
    && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)

  WitnessDelegation{ proof = UCAN.UCAN{ claims = proofClaims }, .. } ->
    -- Proof integrity
         UCAN.sender ucanClaims
    == UCAN.receiver proofClaims
    -- Capability integrity
    && any (== capability) (UCAN.attenuation ucanClaims)
    -- Delegation ability
    && any (`canDelegate` capability) (UCAN.attenuation proofClaims)
    -- Time bounds
    && expiration <= UCAN.expiration ucanClaims
    && fromMaybe True ((>=) <$> notBefore <*> UCAN.notBefore ucanClaims)


checkWitness :: (DelegationSemantics cap, Eq cap) => UCAN fct cap -> Witness fct cap -> Bool
checkWitness ucan witness =
  checkWitnessIntegrity ucan witness
  && case witness of
    WitnessDelegation{..} ->
      checkWitness proof delegation

    _ -> True
