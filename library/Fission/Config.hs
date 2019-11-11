-- | Application configuration and top-level 'RIO' helpers
module Fission.Config
  ( get
  -- , module Fission.Config.Types
  ) where

import RIO

import Data.Has

-- $setup
--
-- >>> import RIO
-- >>> import Data.Has
-- >>>
-- >>> :set -XBlockArguments
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XOverloadedStrings

-- | Get a value from the reader config
--
--   >>> data Field = Field Text
--   >>> data Cfg = Cfg Field
--   >>>
--   >>> instance Has Field Cfg where hasLens = \f (Cfg field) -> Cfg <$> f field
--   >>>
--   >>> :{
--   >>>   runRIO (Cfg $ Field "hello world") do
--   >>>     Field txt <- get
--   >>>     return txt
--   >>> :}
--   "hello world"
get :: (MonadReader cfg m, Has a cfg) => m a
get = view hasLens
