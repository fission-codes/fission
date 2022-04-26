module Web.UCAN.DelegationChain
  ( delegationChains
  , delegationChainStream
  , rootIssuer
  , capabilityCanBeDelegated
  , ownershipCanBeDelegated

  -- Re-exports
  , module Web.UCAN.Capabilities.Class
  , module Web.UCAN.DelegationChain.StreamWithErrors
  , module Web.UCAN.DelegationChain.Types
  ) where

import           Data.Aeson

import           Control.Applicative
import qualified List.Transformer                          as ListT
import           RIO                                       hiding (exp, to)

import qualified Text.URI                                  as URI

import           Web.DID.Types
import           Web.UCAN.Error
import           Web.UCAN.Proof.Class
import           Web.UCAN.Resolver.Class
import           Web.UCAN.Types                            (UCAN)
import qualified Web.UCAN.Types                            as UCAN
import qualified Web.UCAN.Validation                       as Validation

-- Re-exports

import           Control.Monad.Except
import           Web.UCAN.Capabilities.Class
import           Web.UCAN.DelegationChain.StreamWithErrors
import           Web.UCAN.DelegationChain.Types


type MonadDelegationChain m =
  ( MonadError Error m
  , Alternative m
  , Resolver m
  )

type IsCapability res abl =
  ( DelegationSemantics res
  , DelegationSemantics abl
  , Eq res
  , Eq abl
  , IsResource res
  , IsAbility abl
  )


delegationChains ::
  ( IsCapability res abl
  , FromJSON fct
  , Resolver m
  )
  => UCAN fct res abl
  -> m [Either Error (DelegationChain fct res abl)]
delegationChains =
  ListT.fold (flip (:)) [] id . runStreamWithErrors . delegationChainStream


delegationChainStream ::
  ( IsCapability res abl
  , FromJSON fct
  , MonadDelegationChain m
  )
  => UCAN fct res abl
  -> m (DelegationChain fct res abl)
delegationChainStream ucan = do
  liftEither $ Validation.checkPure ucan
  capabilitiesFromParenthood ucan <|> capabilitiesFromDelegations ucan


rootIssuer :: DelegationChain fct res abl -> DID
rootIssuer (DelegatedCapability _ _ ucan IntroducedByParenthood) = UCAN.sender (UCAN.claims ucan)
rootIssuer (DelegatedCapability _ _ _ (Delegated proof))         = rootIssuer proof
rootIssuer (DelegatedOwnership (DelegateAs did _ _ _))         = did
rootIssuer (DelegatedOwnership (DelegateMy _ ucan))            = UCAN.sender (UCAN.claims ucan)


capabilityCanBeDelegated ::
  ( DelegationSemantics res
  , DelegationSemantics abl
  , IsResource res
  )
  => (res, UCAN.Ability abl)
  -> DelegationChain fct res abl
  -> Bool
capabilityCanBeDelegated (resource, ability) = \case
  DelegatedCapability parentResource parentAbility _ _ -> do
    (parentResource, parentAbility) `canDelegate` (resource, ability)

  DelegatedOwnership authnDelegation -> do
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
  -> DelegatedOwnership fct res abl
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
  ListT.select attenuation >>= \case
    UCAN.CapResource resource ability ->
      return $ DelegatedCapability resource ability ucan IntroducedByParenthood

    -- `Nothing` indicates "My resources". `Just` would indicate re-delegating ownership
    UCAN.CapOwnedResources (UCAN.OwnedResources Nothing ownershipScope) ->
      return $ DelegatedOwnership $ DelegateMy ownershipScope ucan

    _ ->
      empty


capabilitiesFromDelegations ::
  ( IsCapability res abl
  , FromJSON fct
  , MonadDelegationChain m
  )
  => UCAN fct res abl
  -> m (DelegationChain fct res abl)
capabilitiesFromDelegations ucan@UCAN.UCAN{ claims = UCAN.Claims{..} } = do
  (proofRef, proofIndex) <- ListT.select (proofs `zip` [(0 :: Natural)..])
  proofResolved <- resolveToken proofRef
  proof <- parseJSONString proofResolved
  liftEither $ Validation.checkDelegation proof ucan
  ListT.select attenuation >>= \case
    -- v0.8.1:
    -- If cap is something like prf:x, thent get prf:x and essentially replace prf:x with each of the caps in the prf(s)
    -- If cap is something like as:<did>:* then check that proofs also contain either as:<did>:* or my:* and the issuer matches
    -- If cap is anything else, just do the normal canDelegate thing.
    UCAN.CapResource resource ability -> do
      delegation <- delegationChainStream proof
      guard $ capabilityCanBeDelegated (resource, ability) delegation
      return $ DelegatedCapability resource ability ucan $ Delegated delegation

    -- we only care about the `Just` case here (as:<did>:..),
    -- because the `Nothing` case (my:..) is covered in `capabilitiesFromParenthood` above
    UCAN.CapOwnedResources (UCAN.OwnedResources (Just did) ownershipScope) -> do
      delegationChainStream proof >>= \case
        DelegatedOwnership authDelegation -> do
          guard $ ownershipCanBeDelegated did ownershipScope authDelegation
          return $ DelegatedOwnership $ DelegateAs did ownershipScope ucan authDelegation

        _ ->
          empty

    UCAN.CapProofRedelegation whichProofs -> do
      case whichProofs of
        UCAN.RedelegateAllProofs ->
          return ()

        UCAN.RedelegateProof idx ->
          guard $ proofIndex == idx

      delegationChainStream proof >>= \case
        delegation@(DelegatedCapability resource ability _ _) ->
          return $ DelegatedCapability resource ability ucan $ Delegated delegation

        _ ->
          empty

    _ ->
      empty


resolveToken :: (Resolver m, MonadError Error m) => UCAN.Proof -> m Text
resolveToken = \case
  UCAN.Nested text -> return text
  UCAN.Reference cid -> do
    resolved <- resolve cid
    case resolved of
      Left err -> throwError $ ResolverError err
      Right bs -> return $ decodeUtf8Lenient bs


parseJSONString ::
  ( FromJSON fct
  , IsResource res
  , IsAbility abl
  , MonadError Error m
  ) => Text -> m (UCAN fct res abl)
parseJSONString token =
  case fromJSON $ String token of
    Error e       -> throwError $ ParseError e
    Success proof -> return proof
