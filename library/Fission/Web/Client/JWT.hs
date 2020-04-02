module Fission.Web.Client.JWT
  ( mkAuthReq
  , getSigAuth
  , getRegisterAuth
  ) where

import qualified Data.ByteString.Lazy   as BS.Lazy
import qualified Data.ByteString.Base64 as Base64

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Servant.Client.Core
 
import           Fission.Prelude

import qualified Fission.Key      as Key
import           Fission.User.DID as DID
 
import           Fission.Key.Asymmetric.Algorithm.Types as Key

import           Fission.Web.Auth.Types                as Auth
import           Fission.Web.Auth.JWT.Types            as JWT
import qualified Fission.Web.Auth.JWT.Header.Typ.Types as JWT.Typ
import qualified Fission.Web.Auth.JWT.Signature.Types  as JWT.Signature

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
  => m (AuthenticatedRequest Auth.RegisterDID)
getRegisterAuth = mkAuthReq >>= \case
  Left err -> throwM err
  Right authReq -> return (mkAuthenticatedRequest () \_ -> authReq)

mkAuthReq ::
  ( MonadIO m
  , MonadTime m
  )
  => m (Either Key.Error (Request -> Request))
mkAuthReq = do
  time <- currentTime
 
  Key.readEd <&> \case
    Left err ->
      Left err
     
    Right sk -> Right \req ->
      let
        rawPK = Ed25519.toPublic sk
 
        did = DID
          { publicKey = Key.Public . decodeUtf8Lenient $ Crypto.unpack rawPK
          , algorithm = Key.Ed25519
          , method    = DID.Key
          }

        claims = JWT.Claims
          { iss = did
          , nbf = Just time
          , exp = addUTCTime (secondsToNominalDiffTime 300) time
          }

        encoded =
          sk
            |> Key.signWith
            |> create claims
            |> encodeToken
            |> decodeUtf8Lenient
      in
        addHeader "Authorization" encoded req

create :: JWT.Claims -> (ByteString -> Ed25519.Signature) -> JWT
create claims signF = JWT {..}
  where
    header     = defaultHeader
    headerRaw  = encodePart header
    claimsRaw  = encodePart claims
    toSign     = headerRaw <> "." <> claimsRaw
    sig        = JWT.Signature.Ed25519 $ signF toSign

defaultHeader :: JWT.Header
defaultHeader =
  JWT.Header
    { typ = JWT.Typ.JWT
    , alg = Key.Ed25519
    , cty = Nothing
    }

encodeToken :: JWT -> ByteString
encodeToken JWT {..} = mconcat
  [ "Bearer "
  , encodePart header
  , "."
  , encodePart claims
  , "."
  , BS.Lazy.toStrict $ encode sig
  ]

encodePart :: ToJSON a => a -> ByteString
encodePart part =
  part
    |> encode
    |> BS.Lazy.toStrict
    |> Base64.encode
