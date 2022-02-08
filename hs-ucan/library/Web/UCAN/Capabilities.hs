{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Capabilities
  ( Witness(..)
  , TestCap(..)
  , capabilities
  ) where

import           Data.Aeson

import           RIO
import qualified RIO.List                as List

import           Data.Monoid
import           Web.DID.Types
import           Web.UCAN.Proof
import           Web.UCAN.Resolver.Class
import           Web.UCAN.Types



newtype TestCap = TestCap Text
  deriving DelegationSemantics via (EqualCanDelegate Text)
  deriving newtype FromJSON
  deriving newtype ToJSON
  deriving stock (Show, Eq, Ord)

data Witness fct cap
  = WitnessParenthood
    { capability :: cap
    , originator :: DID
    }
  | WitnessDelegation
    { capability :: cap
    , proof      :: UCAN fct cap
    , delegation :: Witness fct cap
    }
  deriving (Show, Eq)
  --  | WitnessRightsAmplification
  --   { capability    :: cap
  --   , amplifiedFrom :: NonEmpty (Witness fct cap, UCAN fct cap)
  --   }


capabilities ::
  ( DelegationSemantics cap
  , FromJSON fct
  , FromJSON cap
  , Resolver m
  )
  => UCAN fct cap
  -> m [Witness fct cap]
capabilities UCAN{ claims = Claims{..} } = do
  forM attenuation \capability ->
    tryDelegating capability proofs >>= \case
      Just witness ->
        return witness

      Nothing ->
        return WitnessParenthood
          { capability = capability
          , originator = sender
          }

tryDelegating ::
  forall fct cap m .
  ( DelegationSemantics cap
  , FromJSON fct
  , FromJSON cap
  , Resolver m
  )
  => cap
  -> [Proof]
  -> m (Maybe (Witness fct cap))
tryDelegating cap fromProofs = do
  let
    andThen :: m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
    andThen mma fmb = do
      ma <- mma
      case ma of
        Nothing -> return Nothing
        Just a ->
          fmb a

    resolveProof :: Proof -> m (Maybe Text)
    resolveProof = \case
      Nested text -> return $ Just text
      Reference cid -> do
        resolved <- resolve cid
        case resolved of
          Left _   -> return Nothing
          Right bs -> return $ Just $ decodeUtf8Lenient bs

    parse :: Text -> m (Maybe (UCAN fct cap))
    parse token =
      case fromJSON $ String token of
        Success a -> return $ Just a
        _ -> return Nothing

    makeDelegationWitness :: UCAN fct cap -> Witness fct cap -> Witness fct cap
    makeDelegationWitness ucan delegation =
      WitnessDelegation
        { capability = cap
        , proof = ucan
        , delegation = delegation
        }

    findWitness :: UCAN fct cap -> m (Maybe (Witness fct cap))
    findWitness ucan = do
      caps <- capabilities ucan
      return $ makeDelegationWitness ucan <$> List.find (\witness -> capability witness `canDelegate` cap) caps

  candidates <- fromProofs
    & traverse \prf -> resolveProof prf
      `andThen` parse
      `andThen` findWitness

  return $ getFirst $ foldMap First candidates
