module Migration.Models
  ( MigrationConfig(..)
  , Migration(..)
  , databaseNeedsUpdating
  ) where

data MigrationConfig = MigrationConfig { databaseVersion :: Int
                                       , desiredVersion :: Int
                                       , migrations :: [Migration] }

data Migration = Migration { upSql :: String
                           , downSql :: String }

databaseNeedsUpdating :: MigrationConfig -> Bool
databaseNeedsUpdating config = desiredVersion config > databaseVersion config
