module Web.UCAN.Capabilities.Error
  ( Error(..)
  ) where

import           RIO
import           RIO.Time
import           Web.DID.Types
import qualified Web.UCAN.Resolver.Error as Resolver


data Error
  = ResolveError Resolver.Error
  | ParseError String
  | DelegationIssuerAudienceMismatch DID DID
  | DelegationNotBeforeWitnessExpired UTCTime UTCTime
  | DelegationExpiresAfterNotBefore UTCTime UTCTime
  deriving (Show, Eq, Exception)
