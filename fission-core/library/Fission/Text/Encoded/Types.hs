module Fission.Text.Encoded.Types (Encoded (..)) where

import           Data.Kind

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text as Text

import           Fission.Text.Encoding.Types

-- | Tag some text representation with the base it's encoded with
--   NOTE that this is by no means a perfect method, and is mostly to make
--   tracing encoding through code less painful. Character encoding can always
--   be forgotten, mislabelled, and so on.
newtype (enc :: Encoding) `Encoded` (carrierText :: Type)
  = Encoded { encoded :: carrierText }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance Functor (Encoded enc) where
  fmap f Encoded { encoded } = Encoded $ f encoded

instance Display (Encoded 'UTF8 Text) where
  display Encoded { encoded } = display encoded

instance Display (Encoded 'Octal String) where
  textDisplay Encoded { encoded } = Text.pack encoded

instance Display (Encoded 'Octal ByteString) where
  -- NOTE This function performs no checks to ensure that the data is, in fact, UTF8 encoded
  display Encoded { encoded } = displayBytesUtf8 encoded

instance Display (Encoded 'Octal Lazy.ByteString) where
  -- NOTE This function performs no checks to ensure that the data is, in fact, UTF8 encoded
  display encoded = display $ Lazy.toStrict <$> encoded
