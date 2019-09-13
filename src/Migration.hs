module Migration
    ( runMigrations
    ) where
import qualified Migration.Internal as MI (runMigrations)
import qualified Migration.Data as MD (photoJournalMigrations)
import qualified Database.HDBC as DB (IConnection)

runMigrations :: DB.IConnection conn => conn -> IO ()
runMigrations conn = MI.runMigrations conn MD.photoJournalMigrations
