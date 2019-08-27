module Fission.Platform.Heroku.AddOn.Mutate where

-- import           RIO

-- import           Database.Selda

-- import           Data.UUID (UUID)

-- import           Fission.Internal.Constraint
-- import           Fission.Internal.Orphanage ()

-- import           Fission.Platform.Heroku.AddOn.Selector
-- import qualified Fission.Platform.Heroku.AddOn.Table as Table

-- destroy :: MonadRIO    cfg m
--         => MonadSelda      m
--         => HasLogFunc cfg
--         => UUID
--         -> m Int
-- destroy uuid = do
--   n <- deleteFrom Table.addOns (uuid' `is` uuid)
--   logInfo $ "Deactivated Heroku AddOn: " <> displayShow uuid
--   return $ n
