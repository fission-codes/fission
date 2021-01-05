module Fission.Web.Server.App.Content
  ( module Fission.Web.Server.App.Content.Initializer
  , empty
  ) where

import           Network.IPFS.CID.Types

import           Fission.Web.Server.App.Content.Initializer

empty :: CID
empty = CID "Qmc5m94Gu7z62RC8waSKkZUrCCBJPyHbkpmGzEePxy2oXJ" -- Totally blank
