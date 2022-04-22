module Web.UCAN.DelegationChain
  ( delegationChains
  , delegationChainStream
  , rootIssuer
  , capabilityCanBeDelegated
  , ownershipCanBeDelegated
  
  -- Re-exports
  , module Web.UCAN.Capabilities.Class
  , module Web.UCAN.DelegationChain.Class
  , module Web.UCAN.DelegationChain.Types
  ) where

import           Data.Aeson

import           Control.Applicative
import qualified ListT
import           RIO                         hiding (exp, to)

import qualified Text.URI                    as URI

import           Web.DID.Types
import           Web.UCAN.Error
import           Web.UCAN.Proof.Class
import           Web.UCAN.Resolver.Class
import           Web.UCAN.Types              (UCAN)
import qualified Web.UCAN.Types              as UCAN
import qualified Web.UCAN.Validation         as Validation

-- Re-exports

import           Web.UCAN.Capabilities.Class
import Web.UCAN.DelegationChain.Class
import Web.UCAN.DelegationChain.Types



delegationChains ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , FromJSON fct
  , IsResource res
  , IsAbility abl
  , Resolver m
  )
  => UCAN fct res abl
  -> m [Either Error (DelegationChain fct res abl)]
delegationChains =
  -- toReverseList because order doesn't really matter
  -- and this way we get it in the order they're produced
  -- and it's more performant than ListT.toList
  ListT.toReverseList . runStreamWithErrors . delegationChainStream


delegationChainStream ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , FromJSON fct
  , IsResource res
  , IsAbility abl
  , Resolver m
  , MonadDelegationChain m
  )
  => UCAN fct res abl
  -> m (DelegationChain fct res abl)
delegationChainStream ucan = do
  attemptOrEmit $ Validation.checkPure ucan
  capabilitiesFromParenthood ucan <|> capabilitiesFromDelegations ucan

rootIssuer :: DelegationChain fct res abl -> DID
rootIssuer (DelegatedAuthorization _ _ ucan IntroducedByParenthood) = UCAN.sender (UCAN.claims ucan)
rootIssuer (DelegatedAuthorization _ _ _ (Delegated proof))         = rootIssuer proof
rootIssuer (DelegatedAuthentication (DelegateAs did _ _ _))         = did
rootIssuer (DelegatedAuthentication (DelegateMy _ ucan))            = UCAN.sender (UCAN.claims ucan)


capabilityCanBeDelegated ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , IsResource res
  )
  => (res, UCAN.Ability abl)
  -> DelegationChain fct res abl
  -> Bool
capabilityCanBeDelegated (resource, ability) = \case
  DelegatedAuthorization parentResource parentAbility _ _ -> do
    (parentResource, parentAbility) `canDelegate` (resource, ability)

  DelegatedAuthentication authnDelegation -> do
    let ownershipScope =
          case authnDelegation of
            DelegateAs _ scope _ _ -> scope
            DelegateMy scope _     -> scope

    case ownershipScope of
      UCAN.All -> True
      UCAN.OnlyScheme scheme parentAbility ->
        Just scheme == URI.uriScheme (renderResourceURI resource)
        && parentAbility `canDelegate` ability


ownershipCanBeDelegated ::
  DelegationSemantics abl
  => DID
  -> UCAN.OwnershipScope abl
  -> DelegatedAuthentication fct res abl
  -> Bool
ownershipCanBeDelegated did ownershipScope = \case
  DelegateAs parentDid parentOwnershipScope _ _ ->
    parentDid == did
    && parentOwnershipScope `canDelegate` ownershipScope

  DelegateMy parentOwnershipScope parentUcan ->
    -- in case of my:<scope> the DID is implied to be the UCAN sender
    did == UCAN.sender (UCAN.claims parentUcan)
    && parentOwnershipScope `canDelegate` ownershipScope



-- ㊙️


capabilitiesFromParenthood :: MonadDelegationChain m => UCAN fct res abl -> m (DelegationChain fct res abl)
capabilitiesFromParenthood ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  walk attenuation >>= \case
    UCAN.CapResource resource ability ->
      return $ DelegatedAuthorization resource ability ucan IntroducedByParenthood

    -- `Nothing` indicates "My resources". `Just` would indicate re-delegating ownership
    UCAN.CapOwnedResources (UCAN.OwnedResources Nothing ownershipScope) ->
      return $ DelegatedAuthentication $ DelegateMy ownershipScope ucan

    _ ->
      empty


capabilitiesFromDelegations ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , FromJSON fct
  , IsResource res
  , IsAbility abl
  , Alternative m
  , MonadDelegationChain m
  , Resolver m
  )
  => UCAN fct res abl
  -> m (DelegationChain fct res abl)
capabilitiesFromDelegations ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  (proofRef, proofIndex) <- walk (proofs `zip` [(0 :: Natural)..])
  proofResolved <- attemptOrEmit =<< resolveToken proofRef
  proof <- attemptOrEmit $ parseJSONString proofResolved
  attemptOrEmit $ Validation.checkDelegation proof ucan
  walk attenuation >>= \case
    -- v0.8.1:
    -- If cap is something like prf:x, thent get prf:x and essentially replace prf:x with each of the caps in the prf(s)
    -- If cap is something like as:<did>:* then check that proofs also contain either as:<did>:* or my:* and the issuer matches
    -- If cap is anything else, just do the normal canDelegate thing.
    UCAN.CapResource resource ability -> do
      delegation <- delegationChainStream proof
      guard $ capabilityCanBeDelegated (resource, ability) delegation
      return $ DelegatedAuthorization resource ability ucan $ Delegated delegation

    -- we only care about the `Just` case here (as:<did>:..),
    -- because the `Nothing` case (my:..) is covered in `capabilitiesFromParenthood` above
    UCAN.CapOwnedResources (UCAN.OwnedResources (Just did) ownershipScope) -> do
      delegationChainStream proof >>= \case
        DelegatedAuthentication authDelegation -> do
          guard $ ownershipCanBeDelegated did ownershipScope authDelegation
          return $ DelegatedAuthentication $ DelegateAs did ownershipScope ucan authDelegation

        _ ->
          empty

    UCAN.CapProofRedelegation whichProofs -> do
      case whichProofs of
        UCAN.RedelegateAllProofs ->
          return ()

        UCAN.RedelegateProof idx ->
          guard $ proofIndex == idx

      delegationChainStream proof >>= \case
        delegation@(DelegatedAuthorization resource ability _ _) ->
          return $ DelegatedAuthorization resource ability ucan $ Delegated delegation

        _ ->
          empty

    _ ->
      empty

resolveToken :: Resolver m => UCAN.Proof -> m (Either Error Text)
resolveToken = \case
  UCAN.Nested text -> return $ Right text
  UCAN.Reference cid -> do
    resolved <- resolve cid
    case resolved of
      Left err -> return $ Left $ ResolverError err
      Right bs -> return $ Right $ decodeUtf8Lenient bs


parseJSONString ::
  ( FromJSON fct
  , IsResource res
  , IsAbility abl
  ) => Text -> Either Error (UCAN fct res abl)
parseJSONString token =
  case fromJSON $ String token of
    Error e       -> Left $ ParseError e
    Success proof -> Right proof


