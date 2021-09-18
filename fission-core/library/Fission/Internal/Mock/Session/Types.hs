module Fission.Internal.Mock.Session.Types (Session (..)) where

import           Fission.Prelude

-- | The result of running a mocked test session
data Session effs a = Session
  { effectLog :: [OpenUnion effs] -- ^ List of effects that were run
  , result    :: a                -- ^ Pure return value
  }
