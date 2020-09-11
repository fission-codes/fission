-- |

module Fission.CLI.Key.Store.Ed25519 where

getPublicKey ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Ed25519.PublicKey
getPublicKey = Ed25519.toPublic <$> readEd25519

getSecretKey ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Ed25519.SecretKey
getSecretKey = readKey Ed25519.secretKey

sign ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => ByteString
  -> m Ed25519.Signature
sign bs = do
  sk <- readEd25519
  return $ signWith sk bs
