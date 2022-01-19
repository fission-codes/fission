module Web.Ucan
  ( fromRawContent
  , module Web.Ucan.Types
  ) where

import           Data.Aeson
import           RIO

import           Web.Ucan.Resolver.Error as Resolver
import           Web.Ucan.Types


fromRawContent :: (FromJSON fct, FromJSON rsc) => RawContent -> Either Resolver.Error (Ucan fct rsc)
fromRawContent rawContent =
  case decodeStrict rawContentBS of
    Nothing   -> Left (InvalidJWT rawContentBS)
    Just ucan -> Right ucan
  where
    rawContentBS = encodeUtf8 (unRawContent rawContent)
