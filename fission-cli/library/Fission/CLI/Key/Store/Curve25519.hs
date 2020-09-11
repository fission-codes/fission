-- |

module Fission.CLI.Key.Store.Curve25519 where



getPublicKey ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Curve25519.PublicKey
getPublicKey = Curve25519.toPublic <$> readCurve25519

getSecretKey ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Curve25519.SecretKey
readSecretKey = readKey Curve25519.secretKey
