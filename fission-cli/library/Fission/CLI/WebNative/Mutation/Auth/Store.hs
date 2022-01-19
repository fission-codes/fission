module Fission.CLI.WebNative.Mutation.Auth.Store
  ( getBy
  , getRootUCAN
  , getRootUserProof
  , module Fission.CLI.WebNative.Mutation.Auth.Store.Class
  ) where

import qualified Data.Yaml                                       as YAML
import           RIO.Map                                         as Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Web.DID.Types

import           Fission.Web.Auth.Token.UCAN                     as UCAN
import qualified Fission.Web.Auth.Token.UCAN.Types               as Fission

import qualified Web.UCAN.Resolver                               as UCAN
import qualified Web.UCAN.Resolver.Error                         as Resolver
import           Web.UCAN.Types

import           Fission.Web.Auth.Token.Bearer                   as Bearer
import           Fission.Web.Auth.Token.UCAN.Resource.Types      as UCAN

import           Fission.CLI.Environment                         as Env
import           Fission.CLI.WebNative.Mutation.Auth.Store.Class

getRootUCAN ::
  ( MonadIO          m
  , MonadStore       m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadRaise       m
  , m `Raises` NotFound FilePath
  , m `Raises` YAML.ParseException
  )
  => m (Maybe Bearer.Token) -- NOTE You may be root, hence Maybe
getRootUCAN = do
  logDebug @Text "💪🛂 Getting root UCAN"
  store           <- getAll
  Env {rootProof} <- Env.get
  case rootProof of
    Just cid -> return (store !? cid)
    Nothing  -> return Nothing

getRootUserProof ::
  ( MonadIO          m
  , MonadStore       m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadRaise       m
  , m `Raises` NotFound FilePath
  , m `Raises` YAML.ParseException
  )
  => m Fission.Proof
getRootUserProof = Bearer.toProof <$> getRootUCAN

getBy :: forall m.
  ( MonadStore   m
  , MonadRaise   m
  , UCAN.Resolver m
  , m `Raises` Resolver.Error
  , m `Raises` NotFound Bearer.Token
  )
  => DID
  -> (Scope UCAN.Resource -> Bool)
  -> m Bearer.Token
getBy did matcher = do
  bearerTokens <- getAll

  filterM normalizedMatcher (Map.elems bearerTokens) >>= \case
    []          -> raise $ NotFound @Bearer.Token
    (token : _) -> return token

  where
    normalizedMatcher :: Bearer.Token -> m Bool
    normalizedMatcher Bearer.Token {jwt = jwt@UCAN {claims = Claims {resource}}} = do
      UCAN {claims = Claims {sender}} <- ensureM $ UCAN.getRoot jwt
      if sender == did
        then
          case resource of
            Nothing    -> return False
            Just inner -> return $ matcher inner

        else
          return False
