module Fission.User.Query where -- (bySecret) where

import RIO

import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Storage.Query

import Database.Beam
import Database.Beam.Backend.SQL

import Fission.User.Lens
import Fission.User.Types

-- | Find a user by their account secret
-- bySecret :: Text -> Row s User -> Col s Bool
-- bySecret :: BeamSqlBackend be => Text -> User -> QGenExpr context be s Bool
bySecret secret usr = (usr ^. active ==. isTrue_)
                  &&. (usr ^. secretDigest ==. secret)




-- -- foo :: Col s Bool
-- foo user secret = user ^. secretDigest ==. val_ secret

-- baz :: Col s Bool
-- baz = isTrue_ . active user

-- bar = foo &&. baz
