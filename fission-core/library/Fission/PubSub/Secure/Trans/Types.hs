{-# LANGUAGE UndecidableInstances #-}

module Fission.PubSub.Secure.Trans.Types where

import           Control.Monad.Catch
import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

import           Fission.PubSub.Class
import           Fission.PubSub.DM.Class
import           Fission.PubSub.Secure.Class

newtype SecureSessionT m a = SecureSessionT
  { unSecureSessionT :: ReaderT (Symmetric.Key AES256) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadReader (Symmetric.Key AES256)
    , MonadFail
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadRaise
    , MonadRescue
    , MonadTrans
    , MonadPubSub
    , MonadPubSubDM
    )
