module Web.Ucan
  ( fromRawContent
  , module Web.Ucan.Types
  ) where

import RIO
import Data.Aeson

import Web.Ucan.Types
import Web.Ucan.Resolver.Error as Resolver


fromRawContent :: (FromJSON fct, FromJSON rsc) => RawContent -> Either Resolver.Error (Ucan fct rsc)
fromRawContent rawContent =
  case decodeStrict rawContentBS of
    Nothing   -> Left (InvalidJWT rawContentBS)
    Just ucan -> Right ucan
  where
    rawContentBS = encodeUtf8 (unRawContent rawContent)
