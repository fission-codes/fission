module Fission.Web.User.Client (verify) where

  import RIO
  
  import Servant
  import Servant.Client
  
  import qualified Fission.Web.User as User
  
  verify :: BasicAuthData -> ClientM Bool
  verify = client (Proxy :: Proxy User.VerifyRoute)
  