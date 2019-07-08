module Fission.Config (get) where

import RIO

import Data.Has

-- | Get a value from the reader config
--
--   >>> import Data.Has
--   >>> :set -XMultiParamTypeClasses
--   >>>
--   >>> newtype Example = Example Text deriving Show
--   >>> data ExCfg = ExCfg { example :: Text }
--   >>>
--   >>> instance Has ExCfg Example where hasLens = example
--   >>>
--   >>> runRIO (ExCfg "hello world") (get :: Example)
--   Example "hello world"
get :: (MonadReader cfg m, Has a cfg) => m a
get = view hasLens
