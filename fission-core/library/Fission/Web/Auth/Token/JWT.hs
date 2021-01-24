module Fission.Web.Auth.Token.JWT
  ( getRoot
  , module Fission.Web.Auth.Token.JWT.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Resolver       as JWT
import           Fission.Web.Auth.Token.JWT.Resolver       as Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as Resolver
import           Fission.Web.Auth.Token.JWT.Types

getRoot :: (JWT.Resolver m, MonadLogger m) => JWT -> m (Either Resolver.Error JWT)
getRoot jwt@JWT {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right jwt

    Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) ->
          getRoot proofJWT

        Left err -> do
          logWarn $ "Failed token resolution " <> textDisplay err
          return $ Left err
