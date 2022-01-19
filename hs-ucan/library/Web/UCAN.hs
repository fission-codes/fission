module Web.UCAN
  ( fromRawContent
  , module Web.UCAN.Types
  ) where

import           Data.Aeson
import           RIO

import           Web.UCAN.Resolver.Error as Resolver
import           Web.UCAN.Types


fromRawContent :: (FromJSON fct, FromJSON rsc) => RawContent -> Either Resolver.Error (UCAN fct rsc)
fromRawContent rawContent =
  case decodeStrict rawContentBS of
    Nothing   -> Left (InvalidJWT rawContentBS)
    Just ucan -> Right ucan
  where
    rawContentBS = encodeUtf8 (unRawContent rawContent)
