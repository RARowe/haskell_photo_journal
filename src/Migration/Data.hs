module Migration.Data
  ( photoJournalMigrations
  ) where
import qualified Migration.Models as M

photoJournalMigrations :: [M.Migration]
photoJournalMigrations = [createFilmRollTable]

createFilmRollTable = M.Migration {
    M.upSql = "create table if not exists FilmRoll (Title text not null, DateCreated text not null);"
  , M.downSql = "drop table FilmRoll;"
}
