module Web.UCAN.Capabilities.Error
  ( Error(..)
  ) where

import           RIO
import qualified Web.UCAN.Resolver.Error as Resolver


data Error
  = ResolveError Resolver.Error
  | ParseError String
  deriving (Show, Eq, Exception)
