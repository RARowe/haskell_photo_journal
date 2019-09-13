module Migration.Internal
    ( runMigrations
    ) where
import qualified Migration.Models as M
import qualified Migration.DataAccess as MDA
import qualified Data.Maybe as M (fromMaybe)
import qualified Database.HDBC as DB (IConnection)

runMigrations :: DB.IConnection conn => conn -> [M.Migration] -> IO ()
runMigrations conn ms = do
  migrationVersionTableExists <- MDA.migrationVersionTableExists conn
  case migrationVersionTableExists of
    Just True -> do
      dbVersion <- MDA.retrieveMigrationVersion conn
      runMig M.MigrationConfig { M.databaseVersion = M.fromMaybe 0 dbVersion
                               , M.desiredVersion = desiredVersion
                               , M.migrations = ms }
    Just False -> runMig M.MigrationConfig { M.databaseVersion = 0
                                           , M.desiredVersion = desiredVersion
                                           , M.migrations = freshMigrations }
    Nothing -> return ()
  where runMig = MDA.runMigrationsWithConfig conn
        desiredVersion = length ms
        freshMigrations = createMigrationVersionTable : createMigrationVersionEntry : ms

createMigrationVersionTable :: M.Migration
createMigrationVersionTable = M.Migration { M.upSql = createMigrationVersionTableSql, M.downSql = "" }

createMigrationVersionEntry :: M.Migration
createMigrationVersionEntry = M.Migration { M.upSql = createMigrationVersionEntrySql, M.downSql = "" }

createMigrationVersionTableSql :: String
createMigrationVersionTableSql = "create table MigrationVersion (Version int not null);";

createMigrationVersionEntrySql :: String
createMigrationVersionEntrySql = "insert into MigrationVersion values (0);"
