module Web.UCAN
  ( parse
  , module Web.UCAN.Types
  ) where

import           Data.Aeson

import           RIO
import qualified RIO.Text as Text

import           Web.UCAN.Resolver.Error as Resolver
import           Web.UCAN.Types

parse ::
  ( FromJSON fct
  , FromJSON rsc
  , FromJSON ptc
  )
  => ByteString
  -> Either Resolver.Error (UCAN fct rsc ptc)
parse bs =
  case eitherDecodeStrict bs of
    Left reason -> Left $ InvalidJWT (Text.pack reason) bs
    Right ucan  -> Right ucan
