-- | Application configuration and top-level 'RIO' helpers
module Fission.Config
  ( get
  , module Fission.Config.Types
  ) where

import RIO

import Data.Has
import Fission.Config.Types

-- $setup
-- >>> import Data.Has
-- >>> import qualified Fission.Config as Config
-- >>> :set -XMultiParamTypeClasses

-- | Get a value from the reader config
--
--   >>> newtype Example = Example Text deriving Show
--   >>> data ExCfg = ExCfg { example :: Example }
--   >>> instance Has ExCfg Example where hasLens = example
--   >>>
--   >>> runRIO (ExCfg "hello world") (get :: Example)
--   Example "hello world"
get :: (MonadReader cfg m, Has a cfg) => m a
get = view hasLens
