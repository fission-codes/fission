module Fission.Web.Auth.JWT
  ( handler
  , validateJWT
  , module Fission.Web.Auth.JWT.Types
  , module Fission.Web.Auth.JWT.Error
  ) where

import           Fission.Prelude
import           Fission.Models

import           Database.Esqueleto

import qualified Data.ByteString.Char8  as Ch
import qualified Data.ByteString.Lazy   as BS.Lazy
import qualified Data.ByteString.Base64 as Base64

import qualified Crypto.PubKey.Ed25519 as Ed
import           Crypto.Error

import qualified Fission.Internal.Crypto as Crypto

import           Fission.Time

import           Fission.Web.Auth.JWT.Types as JWT
import           Fission.Web.Auth.JWT.Error as JWT

import qualified Fission.User     as User
import           Fission.User.DID as DID

import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

-- Reexport

import           Fission.Web.Auth.JWT.Types
import           Fission.Web.Auth.JWT.Error

handler ::
  ( MonadTime        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m (Entity User)
handler token@(Auth.Bearer.Token rawToken) =
  validateJWT token >>= \case
    Left err -> do
      logWarn <| "Failed login with token " <> rawToken
      throwM err

    Right did -> do
      runDB <| User.getByDid did >>= \case
        Nothing -> throwM JWT.NoUser
        Just usr -> return usr

validateJWT :: MonadTime m => Auth.Bearer.Token -> m (Either JWT.Error DID)
validateJWT token =
  case parseJWT token of
    Left err -> return <| Left err
    Right pl ->
      validateTime pl >>= \case
        Left err -> return <| Left err
        Right _ -> return <| Right <| iss pl

parseJWT :: Auth.Bearer.Token -> Either JWT.Error JWT.Payload
parseJWT (Auth.Bearer.Token rawToken) = do
  (headerRaw, payloadRaw, sig64) <- getParts rawToken
  void <| validateHeader headerRaw
  payload <- decodePart payloadRaw

  let
    content = headerRaw <> "." <> payloadRaw
    did = iss payload

  pubkey64 <- DID.toPubkey did
  void <| validateSig content pubkey64 sig64

  Right payload

getParts :: ByteString -> Either JWT.Error (ByteString, ByteString, ByteString)
getParts token =
  case Ch.split '.' token of
    [header, payload, sig] -> Right (header, payload, sig)
    _ -> Left ParseError

validateHeader :: ByteString -> Either JWT.Error ()
validateHeader bytes =
  case decodePart bytes of
    Left err -> Left err
    Right header ->
      case (typ header, alg header) of
        ("JWT", "Ed25519") -> Right ()
        ("JWT", _)         -> Left UnsupportedAlg
        _                  -> Left BadHeader

validateTime :: MonadTime m => JWT.Payload -> m (Either JWT.Error ())
validateTime pl = do
  time <- getCurrentPOSIXTime
  return <|
    case (time > JWT.exp pl, time < JWT.nbf pl) of
      (True, _) -> Left JWT.Expired
      (_, True) -> Left JWT.TooEarly
      _         -> Right ()

validateSig :: ByteString -> ByteString -> ByteString -> Either JWT.Error ()
validateSig content pubkey64 sig64 =
  case (Crypto.base64ToEdPubKey pubkey64, Crypto.base64ToSignature sig64) of
    (CryptoPassed pk, CryptoPassed sig) ->
      case Ed.verify pk (Crypto.pack content) sig of
        False -> Left IncorrectSignature
        True -> Right ()
    _ -> Left BadSignature

decodePart :: FromJSON a => ByteString -> Either JWT.Error a
decodePart bytes =
  bytes
    |> Base64.decodeLenient
    |> BS.Lazy.fromStrict
    |> decode
    |> maybe (Left ParseError) Right
