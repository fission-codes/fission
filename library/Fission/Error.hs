module Fission.Error
  ( openLeft
  , relaxedLeft
  , fromMaybe
  , fromMaybe'
  , withRetryM
  , module Fission.Error.Types
  ) where

import Fission.Prelude hiding (fromMaybe)
import Fission.Error.Types

openLeft :: IsMember err errs => err -> Either (OpenUnion errs) a
openLeft err = Left (openUnionLift err)

relaxedLeft :: Contains errsA errsB => OpenUnion errsA -> Either (OpenUnion errsB) a
relaxedLeft = Left . relaxOpenUnion

fromMaybe ::
  IsMember err errs
  => err
  -> (a -> b)
  -> Maybe a
  -> Either (OpenUnion errs) b
fromMaybe err okHandler = maybe (openLeft err) (Right . okHandler)

fromMaybe' :: IsMember err errs => err -> Maybe a -> Either (OpenUnion errs) a
fromMaybe' err = fromMaybe err identity

withRetryM :: Monad m => Natural -> m (Either err a) -> m (Either err a)
withRetryM 0 action = action
withRetryM n action = action >>= \case
  Left  _err -> withRetryM (n - 1) action
  Right val  -> return (Right val)
