module Fission.Web.API.Doc.Types (SwaggerTag) where

-- import           Data.Dynamic
import           Data.Proxy
import           GHC.TypeLits

import           Flow
import           RIO
import qualified RIO.Text        as Text

import           Data.Swagger
import           Servant.Swagger

import           Servant.API

data SwaggerTag (name :: Symbol) (description :: Symbol)
 -- deriving (Typeable)

-- instance HasClient m api => HasClient m (SwaggerTag name description :> api) where
--   type Client m (SwaggerTag name description :> api) = Client m api
--   clientWithRoute _ _ = clientWithRoute (Proxy @m) (Proxy @api)
--   hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

instance
  ( HasSwagger api
  , KnownSymbol name
  , KnownSymbol description
  ) => HasSwagger (SwaggerTag name description :> api) where
  toSwagger _ =
    Proxy @api
      |> toSwagger
      |> applyTags [Tag name' description' Nothing]
    where
      name' =
        Text.pack . symbolVal $ Proxy @name

      description' =
        case symbolVal (Proxy @description) of
          ""  -> Nothing
          str -> Just $ Text.pack str
