module Fission.App.Content
  ( module Fission.App.Content.Initializer
  , empty
  ) where

import           Network.IPFS.CID.Types

import           Fission.App.Content.Initializer

empty :: CID
empty = CID "Qmc5m94Gu7z62RC8waSKkZUrCCBJPyHbkpmGzEePxy2oXJ" -- Totally blank
