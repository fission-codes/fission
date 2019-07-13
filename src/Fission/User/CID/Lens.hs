module Fission.User.CID.Lens where

import RIO

import Control.Lens (makeLenses)

import Fission.User.CID.Types

makeLenses ''UserCID
