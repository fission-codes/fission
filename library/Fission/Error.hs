module Fission.Error
  ( openLeft
  , fromMaybe
  , fromMaybe'
  -- , fromEither
  ) where

import Data.WorldPeace
import RIO hiding (fromMaybe)

openLeft :: IsMember err errs => err -> Either (OpenUnion errs) a
openLeft err = Left (openUnionLift err)

fromMaybe ::
  IsMember err errs
  => err
  -> (a -> b)
  -> Maybe a
  -> Either (OpenUnion errs) b
fromMaybe err okHandler = maybe (openLeft err) (Right . okHandler)

fromMaybe' :: IsMember err errs => err -> Maybe a -> Either (OpenUnion errs) a
fromMaybe' err = fromMaybe err id

-- fromEither :: IsMember err errs => Either err a -> Either (OpenUnion errs) a
-- fromEither (Left  err) = openLeft err
-- fromEither (Right a)   = Right a

-- class Monad m => Raise m where
--   raise :: IsMember err errs => err -> m a

-- instance Raise (Either (OpenUnion errs)) where
--   raise :: IsMember err errs => err -> m a
--   raise err = return <| openLeft err

-- raiseFromEither (Left err)  _    = raise err
-- raiseFromEither (Right val) cont = cont val

-- -----------------

-- class Raise m => Rescue m where
--   rescue :: m a -> (err -> m a) -> m a
