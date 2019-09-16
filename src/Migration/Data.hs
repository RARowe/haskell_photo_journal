module Migration.Data
  ( photoJournalMigrations
  , createTableSql
  ) where
import Data.List (intercalate)
import qualified Migration.Models as M

photoJournalMigrations :: [M.Migration]
photoJournalMigrations = [createFilmRollTable, createPhotoTable]

createFilmRollTable = M.Migration {
    M.upSql = createTableSql "FilmRoll" [ "Title text not null"
                                        , "DateCreated text not null" ]
  , M.downSql = "drop table FilmRoll;"
}

createPhotoTable = M.Migration {
    M.upSql = createTableSql "Photo" [ "FilmRollId integer not null"
                                     , "Description text not null"
                                     , "foreign key(FilmRollId) references FilmRoll(RowId)" ]
  , M.downSql = "drop table Photo;"
}

createTableSql :: String -> [String] -> String
createTableSql tableName columns =
  "create table if not exists " ++ tableName ++ " (" ++ columnString ++ ");"
    where
      columnString = intercalate ", " columns
