module Fission.Internal.MIME
  ( mimeExt
  , lookupExt
  , defaultMimeLookup
  ) where

import RIO
import RIO.Map as Map

import Network.Mime

lookupExt :: MimeType -> Extension
lookupExt mime = fromMaybe "blob" (mimeExt !? mime)

-- FIXME not one-to-one, will probbaly get some funky results
mimeExt :: Map MimeType Extension
mimeExt = Map.fromList $ swap <$> Map.assocs defaultMimeMap
  where
    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)
