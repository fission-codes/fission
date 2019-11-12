module Fission.Internal.MIME
  ( mimeExt
  , lookupExt
  , defaultMimeLookup
  ) where

import Network.Mime
import RIO.Map      as Map

import Fission.Prelude

lookupExt :: MimeType -> Extension
lookupExt mime = fromMaybe "blob" (mimeExt !? mime)

-- NOTE not a one-to-one mapping, so we may get some funky results.
-- Have hand-tested many common types, and am getting the expected extensions.
-- ~BEZ
mimeExt :: Map MimeType Extension
mimeExt = Map.fromList <| swap <$> Map.assocs defaultMimeMap
  where
    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)
