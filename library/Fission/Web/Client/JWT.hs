module Fission.Web.Client.JWT
  ( mkAuthReq
  , getSigAuth
  , getRegisterAuth
  ) where

import           Servant.Client.Core

import qualified Data.ByteString.Lazy   as BS.Lazy
import qualified Data.ByteString.Base64 as Base64

import           Fission.Web.Auth.JWT.Types  as JWT
import           Fission.Web.Auth.Types      as Auth

import qualified Fission.User.DID as DID

import qualified Fission.Key.Store as Key
import qualified Fission.Key.Error as Key

import           Fission.Prelude
import           Fission.Time

import qualified Crypto.PubKey.Ed25519   as Ed
import qualified Fission.Internal.Crypto as Crypto

import qualified Fission.Internal.Orphanage.ClientM ()

getSigAuth ::
  ( MonadIO m
  , MonadTime m
  , MonadThrow m
  )
  => m (AuthenticatedRequest Auth.HigherOrder)
getSigAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest Nothing \_ -> authReq)

getRegisterAuth ::
  ( MonadIO m
  , MonadTime m
  , MonadThrow m
  )
  => m (AuthenticatedRequest Auth.RegisterDid)
getRegisterAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest () \_ -> authReq)

mkAuthReq ::
  ( MonadIO m
  , MonadTime m
  )
  => m (Either Key.Error (Request -> Request))
mkAuthReq = do
  time <- getCurrentPOSIXTime
  Key.readEd >>= return . \case
    Left err -> Left err
    Right sk -> Right \req ->
      let
        pubkey = Ed.toPublic sk
        payload = JWT.Payload
          { iss = DID.fromPubkey pubkey
          , nbf = time
          , exp = time + 300
          }
        token = create payload <| Key.signWith sk
        encoded = decodeUtf8Lenient <| encodeToken token
      in
        addHeader "Authorization" encoded req

create :: JWT.Payload -> (ByteString -> Ed.Signature) -> JWT.Token
create payload signF = JWT.Token {..}
  where
    header     = defaultHeader
    headerRaw  = encodePart header
    payloadRaw = encodePart payload
    toSign     = headerRaw <> "." <> payloadRaw
    sig        = signF toSign

defaultHeader :: JWT.Header
defaultHeader =
  JWT.Header
    { typ = "JWT"
    , alg = "Ed25519"
    }

encodePart :: ToJSON a => a -> ByteString
encodePart part =
  part
    |> encode
    |> BS.Lazy.toStrict
    |> Base64.encode

encodeToken :: JWT.Token -> ByteString
encodeToken token = mconcat
  [ "Bearer "
  , encodePart <| header token
  , "."
  , encodePart <| payload token
  , "."
  , Crypto.toBase64 <| sig token
  ]
