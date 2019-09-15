module Migration.DataAccess
  ( migrationVersionTableExists
  , retrieveMigrationVersion
  , runMigrationsWithConfig
  ) where
import qualified DataSource as DS
import qualified Migration.Models as M
import Database.HDBC (IConnection, toSql)

migrationVersionTableExists :: IConnection conn => conn -> IO (Maybe Bool)
migrationVersionTableExists conn = DS.retrieveSingleBool conn migrationVersionTableExistsSql

retrieveMigrationVersion :: IConnection conn => conn -> IO (Maybe Int)
retrieveMigrationVersion conn = DS.retrieveSingleInt conn databaseMigrationVersionSql

runMigrationsWithConfig :: IConnection conn => conn -> M.MigrationConfig -> IO ()
runMigrationsWithConfig conn config
  | M.databaseNeedsUpdating config = DS.runBatchAndCommit conn $ (updateDatabaseSqlPairs config) ++ [(updateDatabaseVersionSqlPair config)]
  | otherwise = return ()

updateDatabaseSqlPairs :: M.MigrationConfig -> [DS.SqlPair]
updateDatabaseSqlPairs config = map createSqlPair $ migrationsToRun
  where migrationsToRun = drop (M.databaseVersion config) (M.migrations config)
        createSqlPair migration = (M.upSql migration, [])

updateDatabaseVersionSqlPair :: M.MigrationConfig -> DS.SqlPair
updateDatabaseVersionSqlPair config = (updateMigrationVersionSql, [versionSqlValue])
 where versionSqlValue = toSql $ M.desiredVersion config

-- SQL
migrationVersionTableExistsSql :: String
migrationVersionTableExistsSql = "select count(1) from sqlite_master where type='table' and name='MigrationVersion';"

databaseMigrationVersionSql :: String
databaseMigrationVersionSql = "select Version from MigrationVersion limit 1;";

updateMigrationVersionSql :: String
updateMigrationVersionSql = "update MigrationVersion set Version = ?"
