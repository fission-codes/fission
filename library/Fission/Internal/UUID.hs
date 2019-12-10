module Fission.Internal.UUID where

import Data.UUID (UUID)
import qualified Fission.Storage.Database as Database


Database.generateInstances "UUID"
