module Web.UCAN.DelegationChain.Types
  ( DelegationChain(..)
  , ChainStep(..)
  , DelegatedOwnership(..)
  ) where

import           RIO            hiding (exp, to)
import           Web.DID.Types
import           Web.UCAN.Types


data DelegationChain fct res abl
  = DelegatedCapability res (Ability abl) (UCAN fct res abl) (ChainStep (DelegationChain fct res abl))
  | DelegatedOwnership (DelegatedOwnership fct res abl)
  deriving (Show, Eq, Ord)


data ChainStep a
  = IntroducedByParenthood
  | Delegated a
  --  | DelegateViaRightsAmplification (NonEmpty a)
  deriving (Show, Eq, Ord, Functor)


data DelegatedOwnership fct res abl
  = DelegateAs DID (OwnershipScope abl) (UCAN fct res abl) (DelegatedOwnership fct res abl)
  | DelegateMy (OwnershipScope abl) (UCAN fct res abl)
  deriving (Show, Eq, Ord)
