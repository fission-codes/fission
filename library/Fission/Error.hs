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
