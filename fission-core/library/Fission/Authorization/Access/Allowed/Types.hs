{-# LANGUAGE UndecidableInstances #-}

module Fission.Authorization.Access.Allowed.Types (Allowed (..)) where

import           Fission.Prelude

import           Fission.Authorization.PrivilegeFor.Types

data Allowed resource = Allowed
  { lookupID  :: !(LookupData   resource) -- ^ Cache of (e.g.) DB primary key. Doubles as proof that you've actually looked up the resorce
  , privilege :: !(PrivilegeFor resource) -- ^ A *verified* privlege
  }

instance
  ( Eq (PrivilegeFor resource)
  , Eq (LookupData   resource)
  )
  => Eq (Allowed resource) where
    x == y = lookupID  x == lookupID  y && privilege x == privilege y

instance
  ( Show (PrivilegeFor resource)
  , Show (LookupData   resource)) => Show (Allowed resource) where
  show Allowed {..} = mconcat
    [ "Allowed{"
    ,   "lookupID" <> show lookupID <> ","
    ,   "privilege=" <> show privilege
    , "}"
    ]

instance
  ( Display (PrivilegeFor resource)
  , Display (LookupData   resource)
  )
  => Display (Allowed resource) where
    display Allowed {..} = mconcat
      [ "Allowed{"
      ,   "dbID=" <> display lookupID <> ","
      ,   "privilege=" <> display privilege
      , "}"
      ]
