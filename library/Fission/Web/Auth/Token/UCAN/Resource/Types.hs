module Fission.Web.Auth.Token.UCAN.Resource.Types where

import Fission.Prelude
 
import Fission.Models
import Fission.URL

data Resource
  = FissionFileSystem (Scope FilePath)
  | FissionApp        (Scope URL)
  | RegisteredDomain  (Scope URL)
  deriving (Eq, Show)

data Scope subset
  = Complete
  | Subset subset
  deriving (Eq, Show)

instance ToJSON Scope where
  toJSON Complete = String "*"
  toJSON (Subset rawSubset) = toJSON rawSubset
