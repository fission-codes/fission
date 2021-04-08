module Fission.Text.Encoded
  ( forgetEncoding
  -- * Reexports
  , module Fission.Text.Encoded.Class
  , module Fission.Text.Encoded.Types
  , module Fission.Text.Encoding.Types
  ) where

import RIO

import Fission.Text.Encoded.Class
import Fission.Text.Encoded.Types
import Fission.Text.Encoding.Types

forgetEncoding :: enc `Encoded` carrierText -> carrierText
forgetEncoding Encoded { encoded } = encoded
